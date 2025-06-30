module Mybox.Package.ManualVersionSpec where

import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.SpecBase
import Mybox.Prelude
import Mybox.Tracker

newtype DummyPackage
  = DummyPackage {name :: Text}
  deriving (Eq, Ord, Show)

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
  let pkg = DummyPackage "dummy"
  it "is not reported installed initially" $ do
    setRemoteVersion "version1"
    localVersion pkg >>= (`shouldBe` Nothing)
  it "installs and is reported installed after installation" $ do
    setRemoteVersion "version1"
    nullPackageTracker $ install pkg
    hasInstallLog >>= (`shouldBe` True)
    localVersion pkg >>= (`shouldBe` Just "version1")
