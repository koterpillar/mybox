module Mybox.Package.Queue (
  InstallQueue,
  QueueStrategy (..),
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

-- FIXME: install in parallel by default
data QueueStrategy = QParallel | QSequential

queueAction :: Concurrent :> es => QueueStrategy -> Eff es () -> Eff es ()
queueAction QParallel = void . forkIO
queueAction QSequential = void

runInstallQueue ::
  (AppDisplay :> es, Concurrent :> es) =>
  QueueStrategy ->
  Eff (InstallQueue : es) a ->
  Eff es a
runInstallQueue strategy =
  reinterpret (evalState $ mempty @QueueState) $
    \localEnv ->
      \case
        Enqueue items -> do
          res <- for items $ \(pkgName, action) ->
            do
              displayBanner $ bannerPending pkgName
              ps <- stateM $ \qs -> case Map.lookup pkgName qs of
                Just mv -> pure (Left mv, qs)
                Nothing -> do
                  mv <- newEmptyMVar
                  pure (Right mv, Map.insert pkgName mv qs)
              case ps of
                Left mv -> pure mv
                Right mv -> localUnlift localEnv (ConcUnlift Persistent Unlimited) $
                  \unlift -> do
                    queueAction strategy $ unlift action `finally` putMVar mv ()
                    pure mv
          traverse_ readMVar res
