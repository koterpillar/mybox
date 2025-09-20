module Mybox.Package.URLSpec where

import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Archive
import Mybox.Package.Class
import Mybox.Package.SpecBase
import Mybox.Package.System
import Mybox.Package.URL
import Mybox.Prelude
import Mybox.SpecBase

preinstallNode :: App es => Eff es ()
preinstallNode = do
  os <- drvOS
  case os of
    Linux _ -> ensureInstalled $ mkSystemPackage "nodejs"
    MacOS -> ensureInstalled $ mkSystemPackage "node"

spec :: Spec
spec = do
  jsonSpec @URLPackage [(Nothing, "{\"url\": \"https://example.com/package.tar.gz\"}")]
  describe "name" $ do
    it "uses URL to derive name" $ do
      let package = mkURLPackage "https://example.com/package.tar.gz"
      package.name `shouldBe` "example.com/package"
  packageSpec $
    ps ((mkURLPackage "https://yarnpkg.com/latest.tar.gz"){archive = emptyArchiveFields{binaries = ["yarn"], binaryWrapper = True}})
      & preinstall preinstallNode
      & checkInstalledCommandOutput ("yarn" :| ["--help"]) "Usage: yarn"
  packageSpec $
    ps ((mkURLPackage "https://ftp.debian.org/debian/README"){archive = emptyArchiveFields{raw = Right True}})
      & checkInstalledCommandOutput
        (shellRaw "cat ~/.local/mybox/ftp.debian.org--README/README")
        "Debian GNU/Linux"
