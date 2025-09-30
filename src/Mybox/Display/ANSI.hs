{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Display.ANSI (
  supportsANSI,
  ANSIDisplayable,
  runANSIDisplay,
) where

import Data.List (delete)
import Data.Text qualified as Text
import Effectful.Concurrent.MVar
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
  reinterpretWith_
    (evalState @[Banner a] mempty)
    act
    $ \case
      Log log -> locked $ withBanner @(Banner a) $ draw $ terminalShow log
      AddBanner banner -> locked $ withBanner @(Banner a) $ modify (banner :)
      RemoveBanner banner -> locked $ withBanner @(Banner a) $ modify (delete banner)

withBanner ::
  forall banner es r.
  (Monoid banner, Print :> es, State [banner] :> es, TerminalShow banner) =>
  Eff es r ->
  Eff es r
withBanner act = do
  oldBanner <- gets @[banner] mconcat
  eraseBanner oldBanner
  act `finally` do
    newBanner <- gets @[banner] mconcat
    drawBanner newBanner

mlist :: (a -> b) -> Maybe a -> [b]
mlist f = toList . fmap f

draw :: Print :> es => [[TerminalItem]] -> Eff es ()
draw items = do
  forM_ items $ \line -> do
    forM_ line $ \item -> do
      Print.print $
        setSGRCode $
          [Reset]
            ++ mlist (SetColor Foreground Dull) item.foreground
      Print.print $ Text.unpack item.text
    Print.print "\n"

drawBanner :: (Print :> es, TerminalShow banner) => banner -> Eff es ()
drawBanner banner = do
  items <- terminalWrap banner
  draw items
  backLines $ length items

wrapLine :: Int -> [TerminalItem] -> [[TerminalItem]]
wrapLine maxWidth items = go 0 [] [] items
 where
  go :: Int -> [TerminalItem] -> [[TerminalItem]] -> [TerminalItem] -> [[TerminalItem]]
  go _ currentLine acc [] = reverse $ reverse currentLine : acc
  go currentWidth currentLine acc (item : rest) =
    let itemWidth = Text.length item.text
        newWidth = currentWidth + itemWidth
     in if newWidth <= maxWidth || null currentLine
          then go newWidth (item : currentLine) acc rest
          else go itemWidth [item] (reverse currentLine : acc) rest

terminalWrap :: (Print :> es, TerminalShow banner) => banner -> Eff es [[TerminalItem]]
terminalWrap banner = do
  width <- (fromMaybe 80 . fmap fst) <$> Print.terminalSize
  pure $ concatMap (wrapLine width) $ terminalShow banner

eraseBanner :: (Print :> es, TerminalShow banner) => banner -> Eff es ()
eraseBanner banner = do
  lineCount <- length <$> terminalWrap banner
  replicateM_ lineCount $ Print.printLn clearFromCursorToLineEndCode
  backLines lineCount

backLines :: Print :> es => Int -> Eff es ()
backLines n = do
  Print.print $ cursorUpCode n
  Print.print $ setCursorColumnCode 0
  Print.flush
