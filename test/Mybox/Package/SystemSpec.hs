module Mybox.Package.SystemSpec where

import Mybox.Driver
import Mybox.Package.SpecBase
import Mybox.Package.System
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    @SystemPackage
    [ (Nothing, "{\"system\": \"test\"}")
    , (Just "URL", "{\"system\": \"test\", \"url\": \"test\"}")
    , (Just "auto update", "{\"system\": \"test\", \"auto_updates\": true}")
    , (Just "flatpak installer", "{\"system\": \"org.example.Test\", \"installer\": \"flatpak\"}")
    ]
  onlyIf "System package tests require virtual system (Docker or CI)" virtualSystem $ do
    packageSpec $
      ps (mkSystemPackage "ripgrep")
        & checkInstalledCommandOutput
          ("rg" :| ["--help"])
          "ripgrep"
    packageSpec $
      ps (mkSystemPackage "alacritty")
        & ignorePaths [".terminfo"]
        & checkInstalledCommandOutput
          ("alacritty" :| ["--version"])
          "alacritty 0"
    onlyIfOS "RPM is only available on Fedora" (\case Linux Fedora -> True; _ -> False) $
      packageSpec $
        ps ((mkSystemPackage "rpmfusion-free-release"){url = Just "https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-43.noarch.rpm"})
          & checkInstalledCommandOutput
            ("cat" :| ["/etc/yum.repos.d/rpmfusion-free.repo"])
            "RPM Fusion for Fedora"
    onlyIfOS "Apt is only available on Debian" (\case Linux (Debian "debian") -> True; _ -> False) $
      packageSpec $
        ps (mkSystemPackage "openvox-release"){url = Just "https://apt.voxpupuli.org/openvox8-release-debian13.deb"}
          & checkInstalledCommandOutput
            ("cat" :| ["/etc/apt/sources.list.d/openvox8-release.list"])
            "OpenVox 8"
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
    onlyIf "Flatpak package tests require CI environment" inCI $
      skipIf "Flatpak package tests cannot run in Docker" inDocker $
        onlyIfOS "Flatpak package tests are only available on Linux" (\case Linux _ -> True; _ -> False) $
          packageSpec $
            ps ((mkSystemPackage "org.videolan.VLC"){installer = Just Flatpak})
              & checkInstalledCommandOutput
                ("flatpak" :| ["run", "org.videolan.VLC", "--version"])
                "VLC version"
              & ignorePaths [mkPath ".local/share/flatpak"]
    packageSpec $
      ps ((mkSystemPackage "the_silver_searcher"){installer = Just Brew})
        & preinstallEnableSudo
        & checkInstalledCommandOutput
          ("ag" :| ["--version"])
          "ag version"
