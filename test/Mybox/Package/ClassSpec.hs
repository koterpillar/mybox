module Mybox.Package.ClassSpec where

import Data.Map qualified as Map
import Data.Set qualified as Set

import Mybox.Aeson
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

data TestPackage = TestPackage
  { installed :: Bool
  , error_ :: Bool
  }
  deriving (Eq, Show)

defTestPackage :: TestPackage
defTestPackage = TestPackage{installed = False, error_ = False}

instance HasField "name" TestPackage Text where
  getField = const "test"

instance FromJSON TestPackage where
  parseJSON = withObject "TestPackage" $ \o -> do
    installed <- o .: "installed"
    error_ <- o .: "error"
    pure TestPackage{..}

instance ToJSON TestPackage where
  toJSON p =
    object
      [ "installed" .= p.installed
      , "error" .= p.error_
      ]

instance Package TestPackage where
  remoteVersion _ = pure "1"
  localVersion p = pure $ if p.installed then Just "1" else Nothing
  install p = if p.error_ then throwString "Error" else trkAdd p $ pRoot </> "test-file-1"

spec :: Spec
spec = do
  describe "TestPackage" $
    jsonSpec @TestPackage [(Nothing, "{\"installed\": false, \"error\": false}")]
  describe "ensureInstalled" $ do
    let run_ fn initialSet =
          fmap snd
            . stateTracker initialSet
            . fn
            . runInstallQueue QParallel
            . ensureInstalled
    let run = run_ id
    let mkTrk :: Text -> TrackedFiles
        mkTrk f = Map.singleton (pRoot </> f) $ Set.singleton "test"
    let initState = mkTrk "preexisting"

    it "adds tracked files when installing a package" $ do
      state <- run initState defTestPackage
      state.state `shouldBe` mkTrk "test-file-1"

    it "keeps existing files when already installed" $ do
      state <- run initState $ defTestPackage{installed = True}
      state.state `shouldBe` mkTrk "preexisting"

    it "keeps existing files when install errors" $ do
      state <-
        run_
          (`catch` (\(_ :: StringException) -> pure ()))
          initState
          $ defTestPackage{error_ = True}
      state.state `shouldBe` mkTrk "preexisting"
