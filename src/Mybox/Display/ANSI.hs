{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Display.ANSI (
  supportsANSI,
  ANSIDisplayable,
  runANSIDisplay,
) where

import Data.Text qualified as Text
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import System.Console.ANSI
import System.IO
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Display.Print (Print)
import Mybox.Display.Print qualified as Print
import Mybox.Prelude

supportsANSI :: IOE :> es => Handle -> Eff es Bool
supportsANSI = liftIO . hSupportsANSI

runANSIDisplay ::
  forall a es r.
  ( ANSIDisplayable a
  , Print :> es
  ) =>
  Eff (Display a : es) r ->
  Eff es r
runANSIDisplay =
  reinterpret_
    (evalState @(Banner a) mempty)
    $ \case
      Log log -> withBanner @(Banner a) $ draw $ terminalShow log
      SetBanner banner -> withBanner @(Banner a) $ put banner
      GetBanner -> get

withBanner ::
  forall banner es r.
  (Print :> es, State banner :> es, TerminalShow banner) =>
  Eff es r ->
  Eff es r
withBanner act = do
  oldBanner <- get @banner
  eraseBanner oldBanner
  act `finally` do
    newBanner <- get @banner
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
            ++ mlist (SetColor Background Dull) item.background
      Print.print $ Text.unpack item.text
    Print.print "\n"

drawBanner :: (Print :> es, TerminalShow banner) => banner -> Eff es ()
drawBanner banner = do
  let items = terminalShow banner
  draw items
  backLines $ length items

eraseBanner :: (Print :> es, TerminalShow banner) => banner -> Eff es ()
eraseBanner banner = do
  let lineCount = length $ terminalShow banner
  replicateM_ lineCount $ Print.printLn clearFromCursorToLineEndCode
  backLines lineCount

backLines :: Print :> es => Int -> Eff es ()
backLines n = do
  Print.print $ cursorUpCode n
  Print.print $ setCursorColumnCode 0
  Print.flush
