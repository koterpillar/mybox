module Mybox.Display.Simple (runSimpleDisplay) where

import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
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
  Eff (Display a : es) r -> Eff es r
runSimpleDisplay = reinterpret_
  (evalState @(Banner a) mempty)
  $ \case
    Log log -> liftIO $ print log
    SetBanner banner -> do
      put banner
      liftIO $ print banner
    GetBanner -> get
