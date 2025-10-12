module Mybox.Concurrent (
  atomicMVar,
  modifyMVarPure,
) where

import Effectful
import Effectful.Concurrent.MVar

modifyMVarPure :: Concurrent :> es => MVar a -> (a -> a) -> Eff es ()
modifyMVarPure v f = modifyMVar_ v $ pure . f

atomicMVar :: Concurrent :> es => MVar c -> Eff es a -> Eff es a
atomicMVar v act = modifyMVar v $ \c -> do
  r <- act
  pure (c, r)
