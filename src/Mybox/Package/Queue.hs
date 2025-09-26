module Mybox.Package.Queue (
  InstallQueue,
  queueInstall,
  runInstallQueue,
) where

import Data.Set qualified as Set
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared

import Mybox.Display
import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.Queue.Effect
import Mybox.Prelude
import Mybox.Tracker

-- FIXME: Queue all packages first, then start installing
queueInstall :: (App es, Package p) => p -> Eff es ()
queueInstall pkg = do
  alreadyInstalled <- send $ IsInstalled pkg.name
  unless alreadyInstalled $ do
    ensureInstalled pkg
    send $ MarkInstalled pkg.name

type PackageSet = Set Text

runInstallQueue ::
  (AppDisplay :> es, Driver :> es, Tracker :> es) =>
  Eff (InstallQueue : es) a ->
  Eff es a
runInstallQueue act =
  fmap fst
    $ reinterpretWith_
      (runState $ mempty @PackageSet)
      (inject act)
    $ \case
      IsInstalled pkgName -> gets $ Set.member pkgName
      MarkInstalled pkgName -> modify $ Set.insert pkgName
