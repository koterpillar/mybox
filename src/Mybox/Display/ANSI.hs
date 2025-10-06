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

draw :: Print :> es => [TerminalLine] -> Eff es ()
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

wrapLine :: Int -> TerminalLine -> [TerminalLine]
wrapLine maxWidth = fillLines []
 where
  fillLines acc [] = reverse acc
  fillLines acc items = let (line, rest) = go 0 [] items in fillLines (line : acc) rest
  go _ acc [] = (reverse acc, [])
  go w acc (x : xs)
    | lw x > maxWidth =
        let (x1, x2) = tiSplitAt (pred maxWidth - w) x
         in (reverse (x1 : acc), (x2 : xs)) -- single too long item
    | w + lw x > maxWidth = (reverse acc, x : xs) -- current item doesn't fit
    | otherwise = go (w + lw x) (x : acc) xs
  lw item = Text.length item.text

terminalWrap :: (Print :> es, TerminalShow banner) => banner -> Eff es [TerminalLine]
terminalWrap banner = do
  width <- (fromMaybe 80 . fmap snd) <$> Print.terminalSize
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
