module Mybox.Display.Simple (
  runSimpleDisplay,
  runSimpleDisplay',
) where

import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Display.Print
import Mybox.Prelude

runSimpleDisplay ::
  forall a es r.
  (IOE :> es, Monoid (Banner a), Show (Banner a), Show (Log a)) =>
  Eff (Display a : es) r ->
  Eff es r
runSimpleDisplay = runPrint . runSimpleDisplay' . inject @_ @(Display a : Print : _)

runSimpleDisplay' ::
  forall a es r.
  ( Monoid (Banner a)
  , Print :> es
  , Show (Banner a)
  , Show (Log a)
  ) =>
  Eff (Display a : es) r ->
  Eff es r
runSimpleDisplay' =
  reinterpret_
    (evalState @(Banner a) mempty)
    $ \case
      Log log -> send $ Print $ show log
      SetBanner banner -> do
        put banner
        send $ Print $ show banner
      GetBanner -> get
