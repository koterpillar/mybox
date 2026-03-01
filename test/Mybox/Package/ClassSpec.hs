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
  { error_ :: Bool
  , flavour :: Text
  }
  deriving (Eq, Generic, Show)

defTestPackage :: TestPackage
defTestPackage = TestPackage{error_ = False, flavour = ""}

instance HasField "name" TestPackage Text where
  getField = const "test"

instance PackageName TestPackage where
  withoutName = genericWithoutName' []

instance FromJSON TestPackage where
  parseJSON = withObject "TestPackage" $ \o -> do
    error_ <- o .: "error"
    flavour <- o .: "flavour"
    pure TestPackage{..}

instance ToJSON TestPackage where
  toJSON p =
    object
      [ "error" .= p.error_
      , "flavour" .= p.flavour
      ]

testFile :: Driver :> es => Eff es (Path Abs)
testFile = do
  home <- drvHome
  pure $ home </> "test-file-1"

instance Package TestPackage where
  remoteVersion p = pure p.flavour
  localVersion _ = do
    tf <- testFile
    drvIsFile tf >>= \case
      False -> pure Nothing
      True -> Just <$> drvReadFile tf
  install p
    | p.error_ = throwString "Error"
    | otherwise = do
        tf <- testFile
        drvWriteFile tf p.flavour
        trkAdd p tf

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
    jsonSpec @TestPackage [(Nothing, "{\"error\": false, \"flavour\": \"vanilla\"}")]
  describe "PackageHash" $
    jsonSpec @PackageHash [(Nothing, "{\"hash\": \"test\"}")]
  describe "ensureInstalled" $ do
    let mkTrk :: Path Abs -> TrackedFiles -> TrackedFiles
        mkTrk = tfAdd defTestPackage
    let preexistingFile = pRoot </> "preexisting"
    let initState = mempty & mkTrk preexistingFile
    let testHashFile :: Driver :> es => Eff es (Path Abs)
        testHashFile = (\h -> h </> "hashes" </> "test.json") <$> drvMyboxState

    it "adds tracked files and outputs progress when installing a package" $ do
      tf <- testFile
      hf <- testHashFile
      (state, out) <- run_ id initState defTestPackage
      state.state `shouldBe` (mempty & mkTrk tf & mkTrk hf)
      out `shouldBe` "checking test\ninstalling test\ninstalled test\n"

    it "considers default package installed without hash" $ do
      tf <- testFile
      drvWriteFile tf ""
      (state, out) <- run_ id initState $ defTestPackage
      state.state `shouldBe` initState
      out `shouldBe` "checking test\n"

    let customizedPackage = defTestPackage{flavour = "chocolate"}

    it "reinstalls when definition changes" $ do
      tf <- testFile
      drvWriteFile tf ""
      hf <- testHashFile
      (state, out) <- run_ id initState customizedPackage
      state.state `shouldBe` (mempty & mkTrk tf & mkTrk hf)
      out `shouldBe` "checking test\ninstalling test\ninstalled test\n"
      drvReadFile tf >>= (`shouldBe` "chocolate")

    it "considers customized package installed with correct hash" $ do
      tf <- testFile
      drvWriteFile tf "chocolate"
      hf <- testHashFile
      drvWriteBinaryFile hf $ encode $ pkgHash customizedPackage
      (state, out) <- run_ id initState customizedPackage
      state.state `shouldBe` (mempty & mkTrk preexistingFile & mkTrk hf)
      out `shouldBe` "checking test\n"

    it "keeps existing files and outputs progress when install errors" $ do
      (state, out) <-
        run_
          (`catch` (\(_ :: StringException) -> pure ()))
          initState
          $ defTestPackage{error_ = True}
      state.state `shouldBe` (mempty & mkTrk preexistingFile)
      shouldSatisfy out $
        isPrefixOf "checking test\ninstalling test\nerror test: Control.Exception.Safe.throwString called with:\n\nError"
