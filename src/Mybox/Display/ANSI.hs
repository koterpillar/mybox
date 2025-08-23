module Mybox.Display.ANSI where

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
        Log log -> do
          eraseBanner
          draw $ terminalShow log
          get @(Banner a) >>= drawBanner
        SetBanner banner -> do
          eraseBanner
          put banner
          get @(Banner a) >>= drawBanner
        GetBanner -> get
    )

draw :: IOE :> es => [[TerminalItem]] -> Eff es ()
draw items = liftIO $ do
  forM_ items $ \line -> do
    forM_ line $ \item -> do
      Text.putStr item.text
    Text.putStr "\n"

drawBanner :: (IOE :> es, TerminalShow (Banner a)) => Banner a -> Eff es ()
drawBanner banner = do
  let items = terminalShow banner
  draw items
  liftIO $ do
    cursorUp $ length items
    setCursorColumn 0
    hFlush stdout

eraseBanner :: IOE :> es => Eff es ()
eraseBanner = liftIO $ clearFromCursorToLineEnd
