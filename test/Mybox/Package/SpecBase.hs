module Mybox.Package.SpecBase (
  PackageSpecArgs (..),
  PackageSpec,
  ps,
  psName,
  checkInstalled,
  checkInstalledCommandOutput,
  preinstall,
  preinstallPackage,
  ignorePath,
  packageSpec,
) where

import Data.Set qualified as Set
import Data.Text qualified as Text
import System.Random

import Mybox.Driver
import Mybox.Package.Class
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

data PackageSpecArgs = PackageSpecArgs
  { random :: StdGen
  , directory :: Text
  }

mkPSA :: IO PackageSpecArgs
mkPSA = do
  random_ <- newStdGen
  directory <- ("dest-" <>) . Text.pack . show <$> liftIO (randomIO @Int)
  pure $ PackageSpecArgs{random = random_, directory = directory}

psaSpec :: (PackageSpecArgs -> SpecWith d) -> SpecWith d
psaSpec f = runIO mkPSA >>= f

data PackageSpec a = PackageSpec
  { package :: a
  , name_ :: Maybe Text
  , checkInstalled_ :: forall es. (Driver :> es, IOE :> es) => Eff es ()
  , preinstall_ :: forall es. Driver :> es => Eff es ()
  , ignoredPaths_ :: Set Text
  }

ps :: a -> PackageSpec a
ps p =
  PackageSpec
    { package = p
    , name_ = Nothing
    , checkInstalled_ = liftIO $ expectationFailure "checkInstalled not set"
    , preinstall_ = pure ()
    , ignoredPaths_ = Set.fromList [pMyboxState </> "versions"]
    }

type MPS a = PackageSpec a -> PackageSpec a

psName :: Text -> MPS a
psName n s = s{name_ = Just n}

checkInstalled :: (forall es. (Driver :> es, IOE :> es) => Eff es ()) -> MPS a
checkInstalled f s = s{checkInstalled_ = f}

checkInstalledCommandOutput :: Args -> Text -> MPS a
checkInstalledCommandOutput cmd expectedOutput =
  checkInstalled $ do
    actualOutput <- drvRunOutput cmd
    liftIO $ Text.unpack actualOutput `shouldContain` Text.unpack expectedOutput

ignorePath :: Text -> MPS a
ignorePath path s = s{ignoredPaths_ = Set.insert path s.ignoredPaths_}

preinstall :: (forall es. Driver :> es => Eff es ()) -> MPS a
preinstall f s = s{preinstall_ = preinstall_ s >> f}

preinstallPackage :: Package a => a -> MPS a
preinstallPackage p = preinstall $ nullPackageTracker $ install p

packageSpec :: Package a => (PackageSpecArgs -> PackageSpec a) -> Spec
packageSpec makePS =
  around withTestEnv $
    psaSpec $
      \psa -> do
        let s = makePS psa
        let p = s.package
        describe (Text.unpack $ fromMaybe p.name s.name_) $ do
          it "has a name" $ p.name `shouldSatisfy` (not . Text.null)
          it "installs" $ do
            preinstall_ s
            preexistingFiles <- trackableFiles s
            ((), ts) <-
              stateTracker mempty $ trkSession $ trkPackage p $ install p
            checkAllTracked s preexistingFiles ts
            checkInstalled_ s

trackableFiles :: Driver :> es => PackageSpec a -> Eff es (Set Text)
trackableFiles s = do
  home <- drvHome
  let ignored = Set.map (home </>) s.ignoredPaths_
  existing <- drvFind home (mempty{onlyFiles = True})
  pure $ Set.filter (\p -> not $ any (`pUnder` p) ignored) existing

checkAllTracked ::
  (Driver :> es, IOE :> es) =>
  PackageSpec a ->
  Set Text ->
  TrackerState ->
  Eff es ()
checkAllTracked s preexisting ts = do
  current <- trackableFiles s
  let new = Set.difference current preexisting
  let tracked = Set.map (.path) ts.tracked
  let missing = Set.filter (\path -> not $ any (`pUnder` path) tracked) new
  missing `shouldBe` Set.empty
