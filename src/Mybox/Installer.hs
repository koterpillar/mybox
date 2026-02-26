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
import Mybox.Prelude

data InstallerKind = Flatpak | Brew
  deriving (Eq, Generic, Show)

instance FromJSON InstallerKind where
  parseJSON = withText "InstallerKind" $ \case
    "flatpak" -> pure Flatpak
    "brew" -> pure Brew
    other -> fail $ "Unknown installer kind: " <> show other

instance ToJSON InstallerKind where
  toJSON = \case
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
  MacOS -> brew @s
