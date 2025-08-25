{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Display.ANSI (
  supportsANSI,
  runANSIDisplay,
) where

import Data.Text.IO qualified as Text
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import System.Console.ANSI
import System.IO (Handle, hFlush, hPutStrLn, stdout)
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Prelude

supportsANSI :: IOE :> es => Eff es Bool
supportsANSI = liftIO $ hSupportsANSI stdout

runANSIDisplay ::
  ( IOE :> es
  , Monoid (Banner a)
  , TerminalShow (Banner a)
  , TerminalShow (Log a)
  ) =>
  Eff (Display a : es) r ->
  Eff es r
runANSIDisplay = runANSIDisplay' stdout

runANSIDisplay' ::
  forall a es r.
  ( IOE :> es
  , Monoid (Banner a)
  , TerminalShow (Banner a)
  , TerminalShow (Log a)
  ) =>
  Handle ->
  Eff (Display a : es) r ->
  Eff es r
runANSIDisplay' h =
  reinterpret_
    (evalState @(Banner a) mempty)
    ( \case
        Log log -> withBanner @(Banner a) h $ draw h $ terminalShow log
        SetBanner banner -> withBanner @(Banner a) h $ put banner
        GetBanner -> get
    )

withBanner ::
  forall banner es r.
  (IOE :> es, State banner :> es, TerminalShow banner) =>
  Handle ->
  Eff es r ->
  Eff es r
withBanner h act = do
  oldBanner <- get @banner
  eraseBanner h oldBanner
  act `finally` do
    newBanner <- get @banner
    drawBanner h newBanner

mlist :: (a -> b) -> Maybe a -> [b]
mlist f = toList . fmap f

draw :: IOE :> es => Handle -> [[TerminalItem]] -> Eff es ()
draw h items = liftIO $ do
  forM_ items $ \line -> do
    forM_ line $ \item -> do
      hSetSGR h $
        [Reset]
          ++ mlist (SetColor Foreground Dull) item.foreground
          ++ mlist (SetColor Background Dull) item.background
      Text.hPutStr h item.text
    Text.hPutStr h "\n"

drawBanner :: (IOE :> es, TerminalShow banner) => Handle -> banner -> Eff es ()
drawBanner h banner = do
  let items = terminalShow banner
  draw h items
  backLines h $ length items

eraseBanner :: (IOE :> es, TerminalShow banner) => Handle -> banner -> Eff es ()
eraseBanner h banner = do
  let lineCount = length $ terminalShow banner
  replicateM_ lineCount $ do
    liftIO $ hClearFromCursorToLineEnd h
    liftIO $ hPutStrLn h ""
  backLines h lineCount

backLines :: IOE :> es => Handle -> Int -> Eff es ()
backLines h n = liftIO $ do
  hCursorUp h n
  hSetCursorColumn h 0
  hFlush h
