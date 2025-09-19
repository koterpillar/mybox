module Mybox.Package.ClassSpec where

import Data.Map qualified as Map
import Data.Set qualified as Set

import Mybox.Aeson
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

newtype TestPackage = TestPackage
  { mockInstalled :: Bool
  }
  deriving (Eq, Show)

mkTestPackage :: Bool -> TestPackage
mkTestPackage mockInstalled = TestPackage{..}

instance HasField "name" TestPackage Text where
  getField = const "test"

instance FromJSON TestPackage where
  parseJSON = withObject "TestPackage" $ \o -> do
    mockInstalled <- o .: "installed"
    pure TestPackage{..}

instance ToJSON TestPackage where
  toJSON p =
    object
      [ "installed" .= p.mockInstalled
      ]

instance Package TestPackage where
  remoteVersion _ = pure "1"
  localVersion p = pure $ if p.mockInstalled then Just "1" else Nothing
  install p = trkAdd p $ pRoot </> "test-file-1"

spec :: Spec
spec = do
  describe "TestPackage" $ jsonSpec (Nothing @TestPackage) [(Nothing, "{\"installed\": false}")]
  describe "ensureInstalled" $ do
    let run initialSet =
          fmap snd
            . stateTracker initialSet
            . runInstallQueue_
            . ensureInstalled
    let mkTrk = Map.singleton "test" . Set.fromList . map (\f -> pRoot </> f)
    let initState = mkTrk ["preexisting"]

    it "adds tracked files when installing a package" $ do
      state <- run initState $ mkTestPackage False
      state.current `shouldBe` mkTrk ["test-file-1"]

    it "keeps existing files when already installed" $ do
      state <- run initState $ mkTestPackage True
      state.current `shouldBe` mkTrk ["preexisting"]
