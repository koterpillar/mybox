module Mybox.Package.Queue (InstallQueue, queueInstall, runInstallQueue) where

import Data.Set qualified as Set
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local

import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.Queue.Effect
import Mybox.Prelude
import Mybox.Tracker

queueInstall :: (App es, Package p) => p -> Eff es ()
queueInstall pkg = do
  alreadyInstalled <- send $ IsInstalled pkg.name
  unless alreadyInstalled $ do
    ensureInstalled pkg
    send $ MarkInstalled pkg.name

type PackageSet = Set Text

runInstallQueue ::
  (Driver :> es, TrackerSession :> es) =>
  Eff (InstallQueue : es) a ->
  Eff es a
runInstallQueue act =
  reinterpretWith_
    (evalState $ mempty @PackageSet)
    (inject act)
    $ \case
      IsInstalled pkgName -> gets $ Set.member pkgName
      MarkInstalled pkgName -> modify $ Set.insert pkgName
