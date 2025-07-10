module Mybox.Package.ManualVersionSpec where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

data DummyPackage
  = DummyPackage {name :: Text, number :: Text}
  deriving (Eq, Generic, Ord, Show)

instance FromJSON DummyPackage

instance ToJSON DummyPackage

versionFile :: Text
versionFile = "dummy-package-version.txt"

installLogFile :: Text
installLogFile = "dummy-package-install-log.txt"

instance Package DummyPackage where
  remoteVersion _ = drvReadFile versionFile
  localVersion = manualVersion
  install = manualVersionInstall $ \_ -> drvWriteFile installLogFile ""

setRemoteVersion :: Driver :> es => Text -> Eff es ()
setRemoteVersion = drvWriteFile versionFile

hasInstallLog :: Driver :> es => Eff es Bool
hasInstallLog = drvIsFile installLogFile

spec :: Spec
spec = around withTestEnv $ do
  let pkg = DummyPackage "dummy" "one"
  it "is not reported installed initially" $ nullTrackerSession $ runInstallQueue $ do
    setRemoteVersion "version1"
    localVersion pkg >>= (`shouldBe` Nothing)
  it "installs and is reported installed after installation" $ nullTrackerSession $ runInstallQueue $ do
    setRemoteVersion "version1"
    install pkg
    hasInstallLog >>= (`shouldBe` True)
    localVersion pkg >>= (`shouldBe` Just "version1")
  it "is not reported installed when changed" $ nullTrackerSession $ runInstallQueue $ do
    setRemoteVersion "version1"
    install pkg
    let pkg' = pkg{number = "two"}
    localVersion pkg' >>= (`shouldBe` Nothing)
