module Mybox.Package.SpecBase (
  PackageSpecArgs (..),
  PackageSpec,
  ps,
  psName,
  checkInstalled,
  checkInstalledCommandOutput,
  commandHasOutput,
  preinstall,
  cleanup,
  preinstallPackage,
  ignorePath,
  packageSpec,
  jsonSpec,
) where

import Data.Set qualified as Set
import Data.Text qualified as Text
import System.Random

import Mybox.Aeson
import Mybox.Driver
import Mybox.Driver.Test
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores
import Mybox.Tracker

data PackageSpecArgs = PackageSpecArgs
  { random :: StdGen
  , directory :: Path Abs
  , username :: Text
  , architecture :: Architecture
  , os :: OS
  }

mkPSA :: IO PackageSpecArgs
mkPSA = runEff $ testDriver $ do
  random_ <- liftIO newStdGen
  home <- drvHome
  directoryName <- ("dest-" <>) . Text.pack . show <$> randomIO @Int
  let directory = home </> directoryName
  username <- drvUsername
  architecture <- drvArchitecture
  os <- drvOS
  pure $ PackageSpecArgs{random = random_, ..}

psaSpec :: (PackageSpecArgs -> EffSpec es) -> EffSpec es
psaSpec f = runIO mkPSA >>= f

data PackageSpec a = PackageSpec
  { package :: a
  , name_ :: Maybe Text
  , checkInstalled_ :: forall es. (Driver :> es, IOE :> es) => Eff es ()
  , preinstall_ :: forall es. (Driver :> es, Stores :> es) => Eff es ()
  , cleanup_ :: forall es. (Driver :> es, Stores :> es) => Eff es ()
  , ignoredPaths_ :: Set (Path Rel)
  }

ps :: a -> PackageSpec a
ps p =
  PackageSpec
    { package = p
    , name_ = Nothing
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

psName :: Text -> MPS a
psName n s = s{name_ = Just n}

checkInstalled :: (forall es. (Driver :> es, IOE :> es) => Eff es ()) -> MPS a
checkInstalled f s = s{checkInstalled_ = f}

commandHasOutput :: (Driver :> es, IOE :> es) => Args -> Text -> Eff es ()
commandHasOutput cmd expectedOutput = do
  actualOutput <- drvRunOutput cmd
  actualOutput `shouldContainText` expectedOutput

checkInstalledCommandOutput :: Args -> Text -> MPS a
checkInstalledCommandOutput cmd expectedOutput =
  checkInstalled $ commandHasOutput cmd expectedOutput

ignorePath :: Path Rel -> MPS a
ignorePath path s = s{ignoredPaths_ = Set.insert path s.ignoredPaths_}

preinstall :: (forall es. (Driver :> es, Stores :> es) => Eff es ()) -> MPS a
preinstall f s = s{preinstall_ = preinstall_ s >> f}

cleanup :: (forall es. (Driver :> es, Stores :> es) => Eff es ()) -> MPS a
cleanup f s = s{cleanup_ = cleanup_ s >> f}

preinstallPackage :: Package b => b -> MPS a
preinstallPackage p = preinstall $ nullTrackerSession $ runInstallQueue $ ensureInstalled p

packageSpec :: Package a => (PackageSpecArgs -> PackageSpec a) -> Spec
packageSpec makePS =
  psaSpec $
    \psa -> do
      let s = makePS psa
      let p = s.package
      describe (Text.unpack $ fromMaybe p.name s.name_) $ do
        it "has a name" $ p.name `shouldSatisfy` (not . Text.null)
        it "installs" $
          finally (cleanup_ s) $ do
            preinstall_ s
            preexistingFiles <- trackableFiles s
            ((), ts) <-
              stateTracker mempty $ trkSession True $ runInstallQueue $ do
                install p
                checkVersionMatches p
            checkAllTracked s preexistingFiles ts
            checkInstalled_ s

trackableFiles :: Driver :> es => PackageSpec a -> Eff es (Set (Path Abs))
trackableFiles s = do
  home <- drvHome
  let ignored = Set.map (home <//>) s.ignoredPaths_
  existing <- drvFind home (mempty{onlyFiles = True})
  pure $ Set.filter (\p -> not $ any (`pUnder` p) ignored) existing

checkAllTracked ::
  (Driver :> es, IOE :> es) =>
  PackageSpec a ->
  Set (Path Abs) ->
  TrackerState ->
  Eff es ()
checkAllTracked s preexisting ts = do
  current <- trackableFiles s
  let new = Set.difference current preexisting
  let tracked = Set.map (.path) ts.tracked
  let missing = Set.filter (\path -> not $ any (`pUnder` path) tracked) new
  missing `shouldBe` Set.empty

checkVersionMatches :: (App es, IOE :> es, Package p) => p -> Eff es ()
checkVersionMatches p = do
  remote <- remoteVersion p
  local <- localVersion p
  local `shouldBe` Just remote

jsonSpec :: forall proxy a. (Eq a, Package a) => proxy a -> [(Maybe Text, Text)] -> Spec
jsonSpec _ examples = describe "JSON parsing" $ for_ examples $ \(name, json) -> do
  it ("parses" <> Text.unpack (maybe mempty (" " <>) name) <> " and roundtrips") $ do
    let pkgE = jsonDecode @a "example" json
    pkgE `shouldSatisfy` isRight
    pkg <- either (error . show) pure pkgE
    let json' = jsonEncode pkg
    let pkgE' = jsonDecode @a "example" json'
    pkg' <- either (error . show) pure pkgE'
    pkg' `shouldBe` pkg
