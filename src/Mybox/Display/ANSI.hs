module Mybox.Display.ANSI where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
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
  let txt = Text.pack $ show banner
  case pos of
    Nothing -> Text.putStrLn txt -- fallback
    Just (x, _) -> do
      let nl = "\n"
      Text.putStr nl
      Text.putStr txt
      cursorUp $ succ $ Text.count nl txt
      setCursorColumn x
