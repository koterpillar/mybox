{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Installer (
  InstallerKind (..),
  Installer,
  mkInstaller,
  iLatestVersion,
  iInstalledVersion,
  iInstall,
  iInstallURL,
  iUpgrade,
  IsSystemPackage (..),
) where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Installer.Apt
import Mybox.Installer.Brew
import Mybox.Installer.Class
import Mybox.Installer.DNF
import Mybox.Installer.Flatpak
import Mybox.Installer.MacOSCombined
import Mybox.JSONEx
import Mybox.Package.System.Class
import Mybox.Prelude

data InstallerKind = Flatpak | Brew
  deriving (Bounded, Enum, Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via ViaJSONEx InstallerKind

instance JSONEx InstallerKind where
  jsonEx = enumCodec "InstallerKind" $ \case
    Flatpak -> "flatpak"
    Brew -> "brew"

mkInstaller :: forall s es. (Driver :> es, IsSystemPackage s) => Maybe InstallerKind -> Eff es Installer
mkInstaller Nothing = osInstaller @s
mkInstaller (Just Flatpak) = pure $ flatpak @s
mkInstaller (Just Brew) = pure $ brew @s

osInstaller :: forall s es. (Driver :> es, IsSystemPackage s) => Eff es Installer
osInstaller = flip fmap drvOS $ \case
  Linux (Debian _) -> apt
  Linux Fedora -> dnf
  Linux (Generic d) -> terror $ "No installer available for generic Linux: " <> d
  MacOS -> macOSCombined @s
