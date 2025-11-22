module Mybox.Package.BrewRepoSpec where

import Mybox.Package.BrewRepo
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  jsonSpec
    @BrewRepo
    [ (Nothing, "{\"brew_tap\": \"easysoft/tap\"}")
    ]
  onlyIf "Homebrew repository tests require virtual system (Docker or CI)" virtualSystem $ do
    withEff (nullTracker . runInstallQueue) $ do
      it "reports not installed when not tapped" $ do
        enableSudo
        localVersion (mkBrewRepo "denji/nginx") >>= (`shouldBe` Nothing)
    packageSpec $
      ps (mkBrewRepo "easysoft/tap")
        & preinstallEnableSudo
        & checkInstalledCommandOutput ("brew" :| ["info", "qcadmin"]) "qcadmin is an open-source lightweight cli tool for managing quickon"
