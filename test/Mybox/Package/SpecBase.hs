module Mybox.Package.SpecBase
  ( PackageSpecArgs(..)
  , PackageSpec
  , ps
  , psName
  , psCheckInstalled
  , psCheckInstalledCommandOutput
  , psPreinstall
  , psIgnorePath
  , packageSpec
  ) where

import qualified Data.Set            as Set

import qualified Data.Text           as Text

import           Mybox.Driver
import           Mybox.Package.Class
import           Mybox.Prelude
import           Mybox.SpecBase
import           Mybox.Tracker

import           System.Random

data PackageSpecArgs = PackageSpecArgs
  { psaRandom    :: StdGen
  , psaDirectory :: Text
  }

mkPSA :: IO PackageSpecArgs
mkPSA = do
  psaRandom <- newStdGen
  psaDirectory <- ("dest-" <>) . Text.pack . show <$> liftIO (randomIO @Int)
  pure $ PackageSpecArgs {..}

psaSpec :: (PackageSpecArgs -> SpecWith d) -> SpecWith d
psaSpec f = runIO mkPSA >>= f

data PackageSpec a = PackageSpec
  { psPackage         :: a
  , psName_           :: Maybe Text
  , psCheckInstalled_ :: forall es. (Driver :> es, IOE :> es) => Eff es ()
  , psPreinstall_     :: forall es. Driver :> es => Eff es ()
  , psIgnoredPaths_   :: Set Text
  }

ps :: a -> PackageSpec a
ps p =
  PackageSpec
    { psPackage = p
    , psName_ = Nothing
    , psCheckInstalled_ = liftIO $ expectationFailure "psCheckInstalled not set"
    , psPreinstall_ = pure ()
    , psIgnoredPaths_ = Set.empty
    }

type MPS a = PackageSpec a -> PackageSpec a

psName :: Text -> MPS a
psName n s = s {psName_ = Just n}

psCheckInstalled :: (forall es. (Driver :> es, IOE :> es) => Eff es ()) -> MPS a
psCheckInstalled f s = s {psCheckInstalled_ = f}

psCheckInstalledCommandOutput :: Args -> Text -> MPS a
psCheckInstalledCommandOutput cmd expectedOutput =
  psCheckInstalled $ do
    actualOutput <- drvRunOutput cmd
    liftIO $ Text.unpack actualOutput `shouldContain` Text.unpack expectedOutput

psIgnorePath :: Text -> MPS a
psIgnorePath path s = s {psIgnoredPaths_ = Set.insert path (psIgnoredPaths_ s)}

psIsIgnored :: PackageSpec a -> Text -> Bool
psIsIgnored s path = any (`pUnder` path) (psIgnoredPaths_ s)

psPreinstall :: (forall es. (Driver :> es) => Eff es ()) -> MPS a
psPreinstall f s = s {psPreinstall_ = psPreinstall_ s >> f}

packageSpec :: Package a => (PackageSpecArgs -> PackageSpec a) -> Spec
packageSpec makePS =
  around withTestEnv
    $ psaSpec
    $ \psa -> do
        let s = makePS psa
        let p = psPackage s
        describe (Text.unpack $ fromMaybe (pkgName p) (psName_ s)) $ do
          it "has a name" $ pkgName p `shouldSatisfy` (not . Text.null)
          it "installs" $ do
            psPreinstall_ s
            preexistingFiles <- trackableFiles s
            ((), ts) <-
              stateTracker mempty $ trkSession $ trkPackage p $ pkgInstall p
            checkAllTracked s preexistingFiles ts
            psCheckInstalled_ s

trackableFiles :: Driver :> es => PackageSpec a -> Eff es (Set Text)
trackableFiles s = do
  home <- drvHome
  existing <- drvFind home (mempty {foOnlyFiles = True})
  pure $ Set.filter (not . psIsIgnored s) existing

checkAllTracked ::
     (IOE :> es, Driver :> es)
  => PackageSpec a
  -> Set Text
  -> TrackerState
  -> Eff es ()
checkAllTracked s preexisting ts = do
  current <- trackableFiles s
  let new = Set.difference current preexisting
  let tracked = Set.map tfPath $ tsTracked ts
  let missing = Set.filter (\path -> not $ any (`pUnder` path) tracked) new
  missing `shouldBe` Set.empty
