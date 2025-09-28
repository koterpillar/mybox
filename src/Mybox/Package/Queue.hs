module Mybox.Package.Queue (
  InstallQueue,
  queueInstall,
  queueInstallMany,
  runInstallQueue,
) where

import Data.Map qualified as Map
import Effectful.Concurrent
import Effectful.Concurrent.MVar
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared

import Mybox.Display
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.Queue.Effect
import Mybox.Prelude

queueInstallMany :: (App es, Package p) => [p] -> Eff es ()
queueInstallMany pkgs = send $ Enqueue [(pkg.name, ensureInstalled pkg) | pkg <- pkgs]

queueInstall :: (App es, Package p) => p -> Eff es ()
queueInstall = queueInstallMany . pure

type QueueState = Map Text (MVar ())

runInstallQueue ::
  (AppDisplay :> es, Concurrent :> es) =>
  Eff (InstallQueue : es) a ->
  Eff es a
runInstallQueue =
  reinterpret (evalState $ mempty @QueueState) $
    \localEnv ->
      \case
        Enqueue items -> do
          res <- for items $ \(pkgName, action) ->
            do
              ps <- stateM $ \qs -> case Map.lookup pkgName qs of
                Just mv -> pure (Left mv, qs)
                Nothing -> do
                  mv <- newEmptyMVar
                  pure (Right mv, Map.insert pkgName mv qs)
              case ps of
                Left mv -> pure mv
                Right mv -> localUnlift localEnv (ConcUnlift Persistent Unlimited) $
                  \unlift -> do
                    _ <- forkIO $ unlift action `finally` putMVar mv ()
                    pure mv
          traverse_ readMVar res
