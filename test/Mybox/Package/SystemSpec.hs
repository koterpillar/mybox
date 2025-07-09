module Mybox.Package.SystemSpec where

import Mybox.Driver
import Mybox.Package.SpecBase
import Mybox.Package.System
import Mybox.Prelude
import Mybox.SpecBase

mkPackage :: Text -> SystemPackage
mkPackage name = SystemPackage{name, url = Nothing, autoUpdates = False}

spec :: Spec
spec = do
  jsonSpec
    (Nothing @SystemPackage)
    [ (Nothing, "{\"system\": \"test\"}")
    , (Just "URL", "{\"system\": \"test\", \"url\": \"test\"}")
    , (Just "auto update", "{\"system\": \"test\", \"auto_updates\": true}")
    ]
  packageSpec $ \psa ->
    ps (mkPackage "ripgrep")
      & checkInstalledCommandOutput
        ("rg" :| ["--help"])
        "ripgrep"
      & psPendingIf (not psa.virtualSystem)
  packageSpec $ \psa ->
    ps (mkPackage "alacritty")
      & checkInstalledCommandOutput
        ("alacritty" :| ["--version"])
        "alacritty 0"
      & psPendingIf (not psa.virtualSystem)
  onlyIf (drvOS >>= \case Linux Fedora -> pure True; _ -> pure False) $
    packageSpec $ \psa ->
      ps ((mkPackage "rpmfusion-free-release"){url = Just "https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-42.noarch.rpm"})
        & checkInstalledCommandOutput
          ("cat" :| ["/etc/yum.repos.d/rpmfusion-free.repo"])
          "RPM Fusion for Fedora"
        & psPendingIf (not psa.virtualSystem)
  onlyIf (drvOS >>= \case Linux (Debian _) -> pure True; _ -> pure False) $
    packageSpec $ \psa ->
      ps (mkPackage "tzdata")
        & checkInstalledCommandOutput
          ("cat" :| ["/usr/share/doc/tzdata/copyright"])
          "Internet Assigned Numbers Authority"
        & psPendingIf (not psa.virtualSystem)
  onlyIf (drvOS >>= \case Linux _ -> pure True; _ -> pure False) $
    packageSpec $ \psa ->
      ps (mkPackage "g++")
        & checkInstalledCommandOutput
          ("g++" :| ["--version"])
          "Free Software Foundation, Inc."
        & psPendingIf (not psa.virtualSystem)
