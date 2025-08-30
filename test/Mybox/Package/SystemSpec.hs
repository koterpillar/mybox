module Mybox.Package.SystemSpec where

import Mybox.Driver
import Mybox.Package.SpecBase
import Mybox.Package.System
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    (Nothing @SystemPackage)
    [ (Nothing, "{\"system\": \"test\"}")
    , (Just "URL", "{\"system\": \"test\", \"url\": \"test\"}")
    , (Just "auto update", "{\"system\": \"test\", \"auto_updates\": true}")
    ]
  onlyIf "System package tests require virtual system (Docker or CI)" virtualSystem $ do
    packageSpec $
      ps (mkSystemPackage "ripgrep")
        & checkInstalledCommandOutput
          ("rg" :| ["--help"])
          "ripgrep"
    packageSpec $
      ps (mkSystemPackage "alacritty")
        & ignorePath ".terminfo"
        & checkInstalledCommandOutput
          ("alacritty" :| ["--version"])
          "alacritty 0"
    onlyIfOS "RPM Fusion package tests are only available on Fedora" (\case Linux Fedora -> True; _ -> False) $
      packageSpec $
        ps ((mkSystemPackage "rpmfusion-free-release"){url = Just "https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-42.noarch.rpm"})
          & checkInstalledCommandOutput
            ("cat" :| ["/etc/yum.repos.d/rpmfusion-free.repo"])
            "RPM Fusion for Fedora"
    onlyIfOS "Timezone package only requires interactive configuration on Debian" (\case Linux (Debian _) -> True; _ -> False) $
      packageSpec $
        ps (mkSystemPackage "tzdata")
          & checkInstalledCommandOutput
            ("cat" :| ["/usr/share/doc/tzdata/copyright"])
            "Internet Assigned Numbers Authority"
    onlyIfOS "GCC package tests are only available on Linux" (\case Linux _ -> True; _ -> False) $
      packageSpec $
        ps (mkSystemPackage "g++")
          & checkInstalledCommandOutput
            ("g++" :| ["--version"])
            "Free Software Foundation, Inc."
