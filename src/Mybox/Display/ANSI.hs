module Mybox.Display.ANSI where

import Effectful.Dispatch.Dynamic
import System.Console.ANSI
import System.IO (stdout)
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Prelude

supportsANSI :: IOE :> es => Eff es Bool
supportsANSI = liftIO $ hSupportsANSI stdout

runANSIDisplay ::
  forall a es r.
  ( IOE :> es
  , Monoid (Banner a)
  , Show (Banner a)
  , Show (Log a)
  ) =>
  Eff (Display a : es) r ->
  Eff es r
runANSIDisplay =
  handle
    . runDisplayImpl
    . inject @_ @(Display a : DisplayImpl a : _) @_

handle ::
  forall a es r.
  ( IOE :> es
  , Show (Banner a)
  , Show (Log a)
  ) =>
  Eff (DisplayImpl a : es) r ->
  Eff es r
handle = interpret_ $ \case
  DrawLog log -> liftIO $ print log
  DrawBanner banner -> drawBanner banner

drawBanner :: (IOE :> es, Show (Banner a)) => Banner a -> Eff es ()
drawBanner banner = liftIO $ do
  pos <- getCursorPosition
  case pos of
    Nothing -> print banner -- fallback
    Just (x, _) -> do
      putStr "\n"
      print banner
      cursorUp 1
      setCursorColumn x
