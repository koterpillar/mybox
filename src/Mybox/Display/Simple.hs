module Mybox.Display.Simple (runSimpleDisplay) where

import Effectful.Dispatch.Dynamic
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Prelude

runSimpleDisplay ::
  forall a es r.
  ( IOE :> es
  , Monoid (Banner a)
  , Show (Banner a)
  , Show (Log a)
  ) =>
  Eff (Display a : es) r ->
  Eff es r
runSimpleDisplay =
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
  DrawBanner banner -> liftIO $ print banner
