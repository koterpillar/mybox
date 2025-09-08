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

versionFile :: Path Rel
versionFile = "dummy-package-version.txt"

installLogFile :: Path Rel
installLogFile = "dummy-package-install-log.txt"

instance Package DummyPackage where
  remoteVersion _ = do
    home <- drvHome
    drvReadFile $ home <//> versionFile
  localVersion = manualVersion
  install = manualVersionInstall $ \_ -> do
    home <- drvHome
    drvWriteFile (home <//> installLogFile) ""

setRemoteVersion :: Driver :> es => Text -> Eff es ()
setRemoteVersion v = do
  home <- drvHome
  drvWriteFile (home <//> versionFile) v

hasInstallLog :: Driver :> es => Eff es Bool
hasInstallLog = do
  home <- drvHome
  drvIsFile (home <//> installLogFile)

spec :: Spec
spec = withEff (nullTracker . runInstallQueue_) $ do
  let pkg = DummyPackage "dummy" "one"
  it "is not reported installed initially" $ do
    setRemoteVersion "version1"
    localVersion pkg >>= (`shouldBe` Nothing)
  it "installs and is reported installed after installation" $ do
    setRemoteVersion "version1"
    install pkg
    hasInstallLog >>= (`shouldBe` True)
    localVersion pkg >>= (`shouldBe` Just "version1")
  it "is not reported installed when changed" $ do
    setRemoteVersion "version1"
    install pkg
    let pkg' = pkg{number = "two"}
    localVersion pkg' >>= (`shouldBe` Nothing)
