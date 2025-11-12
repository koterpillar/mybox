module Mybox.Package.SpecBase (
  PackageSpecArgs (..),
  PackageSpec,
  ps,
  checkInstalled,
  checkInstalledCommandOutput,
  commandHasOutput,
  preinstall,
  preinstallPackage,
  preinstallEnableSudo,
  enableSudo,
  cleanup,
  ignorePaths,
  packageSpec,
  packageSpecGen,
) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Spec.Utils
import Mybox.SpecBase
import Mybox.Stores
import Mybox.Tracker

data PackageSpecArgs = PackageSpecArgs
  { directory :: Path Abs
  , username :: Text
  , architecture :: Architecture
  , os :: OS
  }

mkPSA :: (Driver :> es, IOE :> es) => Eff es PackageSpecArgs
mkPSA = do
  home <- drvHome
  directoryName <- randomText "dest"
  let directory = home </> directoryName
  username <- drvUsername
  architecture <- drvArchitecture
  os <- drvOS
  pure $ PackageSpecArgs{..}

data PackageSpec a = PackageSpec
  { package :: a
  , checkInstalled_ :: forall es. (Driver :> es, IOE :> es) => Eff es ()
  , preinstall_ :: forall es. App es => Eff es ()
  , cleanup_ :: forall es. (Driver :> es, Stores :> es) => Eff es ()
  , ignoredPaths_ :: Set (Path Rel)
  }

ps :: a -> PackageSpec a
ps p =
  PackageSpec
    { package = p
    , checkInstalled_ = expectationFailure "checkInstalled not set"
    , preinstall_ = pure ()
    , cleanup_ = pure ()
    , ignoredPaths_ =
        Set.fromList
          [ pMyboxState </> "versions"
          , ".cache"
          , mkPath "Library/Caches"
          , mkPath ".local/state/dnf5.log"
          ]
    }

type MPS a = PackageSpec a -> PackageSpec a

checkInstalled :: (forall es. (Driver :> es, IOE :> es) => Eff es ()) -> MPS a
checkInstalled f s = s{checkInstalled_ = f}

commandHasOutput :: (Driver :> es, IOE :> es) => Args -> Text -> Eff es ()
commandHasOutput cmd expectedOutput = do
  actualOutput <- drvRunOutput cmd
  actualOutput `shouldContainText` expectedOutput

checkInstalledCommandOutput :: Args -> Text -> MPS a
checkInstalledCommandOutput cmd expectedOutput =
  checkInstalled $ commandHasOutput cmd expectedOutput

ignorePaths :: Foldable t => t (Path Rel) -> MPS a
ignorePaths paths s = s{ignoredPaths_ = Set.union (Set.fromList $ toList paths) s.ignoredPaths_}

preinstall :: (forall es. App es => Eff es ()) -> MPS a
preinstall f s = s{preinstall_ = preinstall_ s >> f}

preinstallPackage :: Package b => b -> MPS a
preinstallPackage p = preinstall $ ensureInstalled p

enableSudo :: App es => Eff es ()
enableSudo = do
  drvOS >>= \case
    MacOS -> pure ()
    Linux _ -> queueInstall $ mkSystemPackage "sudo"
  user <- drvUsername
  let line = user <> " ALL=(ALL) NOPASSWD: ALL"
  modifyDriver sudo $ drvWriteFile (mkPath @Abs "/etc/sudoers.d/mybox") line

preinstallEnableSudo :: MPS a
preinstallEnableSudo = preinstall enableSudo

cleanup :: (forall es. (Driver :> es, Stores :> es) => Eff es ()) -> MPS a
cleanup f s = s{cleanup_ = cleanup_ s >> f}

packageSpecGen :: Package a => String -> (PackageSpecArgs -> PackageSpec a) -> Spec
packageSpecGen name makePS = do
  describe name $
    it "installs" $ do
      psa <- mkPSA
      let s = makePS psa
      let p = s.package
      finally (cleanup_ s) $ do
        nullTracker $ runInstallQueue $ preinstall_ s

        preexistingFiles <- trackableFiles s
        ((), ts) <-
          stateTracker mempty $ runInstallQueue $ do
            install p
            checkVersionMatches p
        checkAllTracked s preexistingFiles ts.state
        checkInstalled_ s

packageSpec :: Package a => PackageSpec a -> Spec
packageSpec s = packageSpecGen (Text.unpack $ s.package.name) (const s)

trackableFiles :: Driver :> es => PackageSpec a -> Eff es (Set (Path Abs))
trackableFiles s = do
  home <- drvHome
  let ignored = Set.map (home <//>) s.ignoredPaths_
  existing <- drvFind home (findOptions{onlyFiles = True})
  pure $ Set.filter (\p -> not $ any (`pUnder` p) ignored) existing

checkAllTracked ::
  (Driver :> es, IOE :> es) =>
  PackageSpec a ->
  Set (Path Abs) ->
  TrackedFiles ->
  Eff es ()
checkAllTracked s preexisting ts = do
  current <- trackableFiles s
  let new = Set.difference current preexisting
  let tracked = Map.keys ts
  let missing = Set.filter (\path -> not $ any (`pUnder` path) tracked) new
  let sample = Set.take 10 missing
  sample `shouldBe` Set.empty

checkVersionMatches :: (App es, IOE :> es, Package p) => p -> Eff es ()
checkVersionMatches p = do
  remote <- remoteVersion p
  local <- localVersion p
  local `shouldBe` Just remote
