module Mybox.Package.ManualVersionSpec where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.ManualVersion.Internal
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

newtype DummyPackage = DummyPackage {name :: Text}
  deriving (Eq, Generic, Ord, Show)

instance FromJSON DummyPackage

instance ToJSON DummyPackage

instance PackageName DummyPackage where
  withoutName = genericWithoutName

dummyVersionFile :: Path Rel
dummyVersionFile = "dummy-package-version.txt"

installLogFile :: Path Rel
installLogFile = "dummy-package-install-log.txt"

instance Package DummyPackage where
  remoteVersion _ = do
    home <- drvHome
    drvReadFile $ home <//> dummyVersionFile
  localVersion = manualVersion
  install = manualVersionInstall $ \_ -> do
    home <- drvHome
    drvWriteFile (home <//> installLogFile) ""

setRemoteVersion :: Driver :> es => Text -> Eff es ()
setRemoteVersion v = do
  home <- drvHome
  drvWriteFile (home <//> dummyVersionFile) v

hasInstallLog :: Driver :> es => Eff es Bool
hasInstallLog = do
  home <- drvHome
  drvIsFile (home <//> installLogFile)

spec :: Spec
spec = do
  describe "ManualVersion" $
    jsonSpec @ManualVersion [(Nothing, "{\"version\": \"test\"}")]
  describe "DummyPackage" $
    metaSpec @DummyPackage [(Nothing, "{\"name\": \"test\"}")]
  withEff (nullTracker . runInstallQueue) $ do
    let pkg = DummyPackage "dummy"
    it "is not reported installed initially" $ do
      setRemoteVersion "version1"
      localVersion pkg >>= (`shouldBe` Nothing)
    it "installs and is reported installed after installation" $ do
      setRemoteVersion "version1"
      install pkg
      hasInstallLog >>= (`shouldBe` True)
      localVersion pkg >>= (`shouldBe` Just "version1")
