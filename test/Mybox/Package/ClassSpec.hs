module Mybox.Package.ClassSpec where

import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Mybox.Aeson
import Mybox.Display
import Mybox.Display.SpecUtils
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores
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

run_ ::
  (AppDisplay :> es', Concurrent :> es, Concurrent :> es', Driver :> es', Stores :> es', Tracker :> es') =>
  (Eff es' () -> Eff (Tracker : AppDisplay : es) ()) ->
  TrackedFiles ->
  TestPackage ->
  Eff es (TrackResult, String)
run_ fn initialSet =
  fmap (\(((), trk), out) -> (trk, out))
    . runSimpleDisplayPure
    . stateTracker initialSet
    . fn
    . runInstallQueue
    . ensureInstalled

spec :: Spec
spec = do
  describe "TestPackage" $
    jsonSpec @TestPackage [(Nothing, "{\"installed\": false, \"error\": false}")]
  describe "ensureInstalled" $ do
    let mkTrk :: Text -> TrackedFiles
        mkTrk f = Map.singleton (pRoot </> f) $ Set.singleton "test"
    let initState = mkTrk "preexisting"

    it "adds tracked files and outputs progress when installing a package" $ do
      (state, out) <- run_ id initState defTestPackage
      state.state `shouldBe` mkTrk "test-file-1"
      out `shouldBe` "checking test\ninstalling test\ninstalled test\n"

    it "keeps existing files and outputs progress when already installed" $ do
      (state, out) <- run_ id initState $ defTestPackage{installed = True}
      state.state `shouldBe` mkTrk "preexisting"
      out `shouldBe` "checking test\n"

    it "keeps existing files and outputs progress when install errors" $ do
      (state, out) <-
        run_
          (`catch` (\(_ :: StringException) -> pure ()))
          initState
          $ defTestPackage{error_ = True}
      state.state `shouldBe` mkTrk "preexisting"
      shouldSatisfy out $
        isPrefixOf "checking test\ninstalling test\nerror test: Control.Exception.Safe.throwString called with:\n\nError"
