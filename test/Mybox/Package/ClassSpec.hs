module Mybox.Package.ClassSpec where

import Data.Set qualified as Set

import Mybox.Aeson
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

data TestPackage = TestPackage
  { mockInstalled :: Bool
  , mockError :: Bool
  }
  deriving (Eq, Show)

mkTestPackage :: Bool -> TestPackage
mkTestPackage mockInstalled = TestPackage{..}
 where
  mockError = False

instance HasField "name" TestPackage Text where
  getField = const "test"

instance FromJSON TestPackage where
  parseJSON = withObject "TestPackage" $ \o -> do
    mockInstalled <- o .: "installed"
    mockError <- o .: "error"
    pure TestPackage{..}

instance ToJSON TestPackage where
  toJSON p =
    object
      [ "installed" .= p.mockInstalled
      , "error" .= p.mockError
      ]

instance Package TestPackage where
  remoteVersion _ = pure "1"
  localVersion p = pure $ if p.mockInstalled then Just "1" else Nothing
  install p = trkAdd p $ pRoot </> "test-file-1"

spec :: Spec
spec = withEff (nullTrackerSession . runInstallQueue_) $ do
  describe "ensureInstalled" $ do
    let run initialSet =
          fmap snd
            . stateTracker initialSet
            . trkSession
            . ensureInstalled
    let mkTrk p = TrackedFile "test" $ pRoot </> p
    let initState = Set.fromList [mkTrk "preexisting"]

    it "adds tracked files when installing a package" $ do
      state <- run initState $ mkTestPackage False
      state.tracked `shouldBe` Set.fromList [mkTrk "test-file-1"]

    it "keeps existing files when already installed" $ do
      state <- run initState $ mkTestPackage True
      state.tracked `shouldBe` Set.fromList [mkTrk "preexisting"]
