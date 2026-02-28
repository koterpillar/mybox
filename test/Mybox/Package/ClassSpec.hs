module Mybox.Package.ClassSpec where

import Data.List (isPrefixOf)

import Mybox.Aeson
import Mybox.Display
import Mybox.Display.SpecUtils
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Hash
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores
import Mybox.Tracker
import Mybox.Tracker.Internal (tfAdd)

data TestPackage = TestPackage
  { installed :: Bool
  , error_ :: Bool
  , flavour :: Text
  }
  deriving (Eq, Generic, Show)

defTestPackage :: TestPackage
defTestPackage = TestPackage{installed = False, error_ = False, flavour = "vanilla"}

instance HasField "name" TestPackage Text where
  getField = const "test"

instance PackageName TestPackage where
  withoutName = genericWithoutName' []

instance FromJSON TestPackage where
  parseJSON = withObject "TestPackage" $ \o -> do
    installed <- o .: "installed"
    error_ <- o .: "error"
    flavour <- o .: "flavour"
    pure TestPackage{..}

instance ToJSON TestPackage where
  toJSON p =
    object
      [ "installed" .= p.installed
      , "error" .= p.error_
      , "flavour" .= p.flavour
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
    jsonSpec @TestPackage [(Nothing, "{\"installed\": false, \"error\": false, \"flavour\": \"vanilla\"}")]
  describe "PackageHash" $
    jsonSpec @PackageHash [(Nothing, "{\"hash\": \"test\"}")]
  describe "ensureInstalled" $ do
    let mkTrk' :: Path Abs -> TrackedFiles -> TrackedFiles
        mkTrk' = tfAdd defTestPackage
    let mkTrk :: Text -> TrackedFiles -> TrackedFiles
        mkTrk f = mkTrk' (pRoot </> f)
    let initState = mempty & mkTrk "preexisting"
    let testHashFile :: Driver :> es => Eff es (Path Abs)
        testHashFile = (\h -> h </> "hashes" </> "test.json") <$> drvMyboxState

    it "adds tracked files and outputs progress when installing a package" $ do
      hf <- testHashFile
      (state, out) <- run_ id initState defTestPackage
      state.state `shouldBe` (mempty & mkTrk "test-file-1" & mkTrk' hf)
      out `shouldBe` "checking test\ninstalling test\ninstalled test\n"

    it "outputs progress when definition changes" $ do
      hf <- testHashFile
      (state, out) <- run_ id initState defTestPackage{flavour = "chocolate"}
      state.state `shouldBe` (mempty & mkTrk "test-file-1" & mkTrk' hf)
      out `shouldBe` "checking test\ninstalling test\ninstalled test\n"

    it "keeps existing files and outputs progress when already installed" $ do
      hf <- testHashFile
      _ <- run_ id initState defTestPackage{installed = True}
      (state, out) <- run_ id initState $ defTestPackage{installed = True}
      state.state `shouldBe` (initState & mkTrk' hf)
      out `shouldBe` "checking test\n"

    it "keeps existing files and outputs progress when install errors" $ do
      (state, out) <-
        run_
          (`catch` (\(_ :: StringException) -> pure ()))
          initState
          $ defTestPackage{error_ = True}
      state.state `shouldBe` (mempty & mkTrk "preexisting")
      shouldSatisfy out $
        isPrefixOf "checking test\ninstalling test\nerror test: Control.Exception.Safe.throwString called with:\n\nError"
