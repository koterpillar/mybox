module Mybox.Package.BrewRepoSpec where

import Mybox.Package.BrewRepo
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    @BrewRepo
    [ (Nothing, "{\"brew_tap\": \"easysoft/tap\"}")
    ]
  onlyIf "Homebrew repository tests require virtual system (Docker or CI)" virtualSystem $
    packageSpec $
      ps (mkBrewRepo "easysoft/tap")
        & preinstallEnableSudo
        & checkInstalledCommandOutput ("brew" :| ["info", "qcadmin"]) "qcadmin is an open-source lightweight cli tool for managing quickon"
