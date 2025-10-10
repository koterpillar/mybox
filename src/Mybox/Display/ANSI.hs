{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Display.ANSI (
  supportsANSI,
  ANSIDisplayable,
  runANSIDisplay,
) where

import Data.List (delete)
import Data.Text qualified as Text
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared
import System.Console.ANSI
import System.IO
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Display.Print (Print)
import Mybox.Display.Print qualified as Print
import Mybox.Prelude

supportsANSI :: IOE :> es => Handle -> Eff es Bool
supportsANSI = liftIO . hSupportsANSI

newtype Lock = Lock (forall es a. Concurrent :> es => Eff es a -> Eff es a)

mkLock :: Concurrent :> es => Eff es Lock
mkLock = do
  lock <- newMVar ()
  pure $ Lock $ withMVar lock . const

data AStateContents a = AStateContents
  { banners :: [Banner a]
  , lines :: Int
  }

emptyAState :: AStateContents a
emptyAState = AStateContents mempty 0

type AState a es = (State (AStateContents a) :> es, Monoid (Banner a), TerminalShow (Banner a))

runANSIDisplay ::
  forall a es r.
  ( ANSIDisplayable a
  , Concurrent :> es
  , Print :> es
  ) =>
  Eff (Display a : es) r ->
  Eff es r
runANSIDisplay act = do
  Lock locked <- mkLock
  (r, s) <- reinterpretWith_
    (runState $ emptyAState @a)
    act
    $ \case
      Log log -> locked $ modifyBanner @a log id
      AddBanner banner -> locked $ modifyBanner @a () (banner :)
      RemoveBanner banner -> locked $ modifyBanner @a () $ delete banner
  -- move cursor after the banner
  replicateM_ s.lines $ Print.printLn ""
  Print.flush
  pure r

drawOver :: forall a es. (AState a es, Print :> es) => [TerminalLine] -> Eff es ()
drawOver content = do
  oldState <- get @(AStateContents a)
  let newHeight = length content
  let erasingLines = max 0 $ oldState.lines - newHeight
  let content' = content <> replicate erasingLines mempty
  draw content'
  upLines erasingLines

modifyBanner ::
  forall a extra es.
  (AState a es, Print :> es, TerminalShow extra) =>
  extra ->
  ([Banner a] -> [Banner a]) ->
  Eff es ()
modifyBanner extra f = do
  oldState <- get @(AStateContents a)
  let newBanner = f oldState.banners
  -- FIXME: tmux test rendering runs without a tty and can't report the width
  width <- Just . fromMaybe 80 <$> Print.terminalWidth
  let newBannerWrapped = terminalShow width $ mconcat newBanner
  let newContents = terminalShow width extra <> newBannerWrapped
  drawOver @a newContents
  let newLines = length newBannerWrapped
  upLines newLines
  put $ oldState{banners = newBanner, lines = newLines}

draw :: Print :> es => [TerminalLine] -> Eff es ()
draw items = do
  forM_ items $ \line -> do
    forM_ line $ \item -> do
      Print.print $
        setSGRCode $
          [Reset]
            ++ toList (SetColor Foreground Dull <$> item.foreground)
      Print.print $ Text.unpack item.text
    Print.print clearFromCursorToLineEndCode
    Print.print "\n"

upLines :: Print :> es => Int -> Eff es ()
upLines n = do
  Print.print $ cursorUpCode n
  Print.print $ setCursorColumnCode 0
  Print.flush
