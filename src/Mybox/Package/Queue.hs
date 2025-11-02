module Mybox.Package.Queue (
  InstallQueue,
  queueInstall,
  queueInstallMany,
  runInstallQueue,
) where

import Data.Functor.Identity
import Data.Map qualified as Map
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared

import Mybox.Display
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.Queue.Effect
import Mybox.Prelude

queueInstallMany :: (App es, Package p, Traversable f) => f p -> Eff es ()
queueInstallMany pkgs = do
  rs <- send $ Enqueue $ fmap (\pkg -> (pkg.name, ensureInstalled pkg)) pkgs
  for_ rs $ either throwIO pure

queueInstall :: (App es, Package p) => p -> Eff es ()
queueInstall = queueInstallMany . Identity

type QueueState = Map Text (MVar IQResult)

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
                    void $ forkIO $ flip withException (putMVar mv . Left) $ do
                      unlift action
                      putMVar mv $ Right ()

                    pure mv
          traverse readMVar res
