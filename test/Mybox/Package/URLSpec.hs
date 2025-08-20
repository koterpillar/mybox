module Mybox.Package.URLSpec where

import Mybox.Driver
import Mybox.Package.Archive
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Package.System
import Mybox.Package.URL
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores
import Mybox.Tracker

preinstallNode :: (Driver :> es, Stores :> es) => Eff es ()
preinstallNode = nullTrackerSession $ runInstallQueue $ do
  os <- drvOS
  case os of
    Linux _ -> ensureInstalled $ mkSystemPackage "nodejs"
    MacOS -> ensureInstalled $ mkSystemPackage "node"

spec :: Spec
spec = do
  jsonSpec (Nothing @URLPackage) [(Nothing, "{\"url\": \"https://example.com/package.tar.gz\"}")]
  describe "name" $ do
    it "uses URL to derive name" $ do
      let package = mkURLPackage "https://example.com/package.tar.gz"
      package.name `shouldBe` "example.com/package"
  packageSpec $ \_ ->
    ps ((mkURLPackage "https://yarnpkg.com/latest.tar.gz"){archive = emptyArchiveFields{binaries = ["yarn"], binaryWrapper = True}})
      & preinstall preinstallNode
      & checkInstalledCommandOutput ("yarn" :| ["--help"]) "Usage: yarn"
  packageSpec $ \_ ->
    ps ((mkURLPackage "https://ftp.debian.org/debian/README"){archive = emptyArchiveFields{raw = Right True}})
      & checkInstalledCommandOutput
        (shellRaw "cat ~/.local/mybox/ftp.debian.org--README/README")
        "Debian GNU/Linux"
