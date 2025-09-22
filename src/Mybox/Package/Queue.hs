module Mybox.Package.Queue (
  InstallQueue,
  queueInstall,
  runInstallQueue,
  runInstallQueue_,
) where

import Data.Set qualified as Set
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared

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
  (Driver :> es, Tracker :> es) =>
  Eff (InstallQueue : es) a ->
  Eff es (a, Set Text)
runInstallQueue act =
  reinterpretWith_
    (runState $ mempty @PackageSet)
    (inject act)
    $ \case
      IsInstalled pkgName -> gets $ Set.member pkgName
      MarkInstalled pkgName -> modify $ Set.insert pkgName

runInstallQueue_ ::
  (Driver :> es, Tracker :> es) =>
  Eff (InstallQueue : es) a ->
  Eff es a
runInstallQueue_ = fmap fst . runInstallQueue
