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
  onlyIf virtualSystem $ do
    packageSpec $ \_ ->
      ps (mkSystemPackage "ripgrep")
        & checkInstalledCommandOutput
          ("rg" :| ["--help"])
          "ripgrep"
    packageSpec $ \_ ->
      ps (mkSystemPackage "alacritty")
        & ignorePath ".terminfo"
        & checkInstalledCommandOutput
          ("alacritty" :| ["--version"])
          "alacritty 0"
    onlyIfOS (\case Linux Fedora -> True; _ -> False) $
      packageSpec $ \_ ->
        ps ((mkSystemPackage "rpmfusion-free-release"){url = Just "https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-42.noarch.rpm"})
          & checkInstalledCommandOutput
            ("cat" :| ["/etc/yum.repos.d/rpmfusion-free.repo"])
            "RPM Fusion for Fedora"
    onlyIfOS (\case Linux (Debian _) -> True; _ -> False) $
      packageSpec $ \_ ->
        ps (mkSystemPackage "tzdata")
          & checkInstalledCommandOutput
            ("cat" :| ["/usr/share/doc/tzdata/copyright"])
            "Internet Assigned Numbers Authority"
    onlyIfOS (\case Linux _ -> True; _ -> False) $
      packageSpec $ \_ ->
        ps (mkSystemPackage "g++")
          & checkInstalledCommandOutput
            ("g++" :| ["--version"])
            "Free Software Foundation, Inc."
