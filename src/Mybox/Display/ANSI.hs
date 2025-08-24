{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Display.ANSI (
  supportsANSI,
  runANSIDisplay,
) where

import Data.Text.IO qualified as Text
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import System.Console.ANSI
import System.IO (hFlush, stdout)
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Prelude

supportsANSI :: IOE :> es => Eff es Bool
supportsANSI = liftIO $ hSupportsANSI stdout

runANSIDisplay ::
  forall a es r.
  ( IOE :> es
  , Monoid (Banner a)
  , TerminalShow (Banner a)
  , TerminalShow (Log a)
  ) =>
  Eff (Display a : es) r ->
  Eff es r
runANSIDisplay =
  reinterpret_
    (evalState @(Banner a) mempty)
    ( \case
        Log log -> withBanner @(Banner a) $ draw $ terminalShow log
        SetBanner banner -> withBanner @(Banner a) $ put banner
        GetBanner -> get
    )

withBanner ::
  forall banner es r.
  (IOE :> es, State banner :> es, TerminalShow banner) =>
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

draw :: IOE :> es => [[TerminalItem]] -> Eff es ()
draw items = liftIO $ do
  forM_ items $ \line -> do
    forM_ line $ \item -> do
      setSGR $
        [Reset]
          ++ mlist (SetColor Foreground Dull) item.foreground
          ++ mlist (SetColor Background Dull) item.background
      Text.putStr item.text
    Text.putStr "\n"

drawBanner :: (IOE :> es, TerminalShow banner) => banner -> Eff es ()
drawBanner banner = do
  let items = terminalShow banner
  draw items
  backLines $ length items

eraseBanner :: (IOE :> es, TerminalShow banner) => banner -> Eff es ()
eraseBanner banner = do
  let lineCount = length $ terminalShow banner
  replicateM_ lineCount $ do
    liftIO $ clearFromCursorToLineEnd
    liftIO $ putStrLn ""
  backLines lineCount

backLines :: IOE :> es => Int -> Eff es ()
backLines n = liftIO $ do
  cursorUp n
  setCursorColumn 0
  hFlush stdout
