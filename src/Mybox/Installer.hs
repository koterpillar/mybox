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
import Mybox.Installer.Mas
import Mybox.Prelude

data InstallerKind = Flatpak | Brew | Mas
  deriving (Eq, Generic, Show)

instance FromJSON InstallerKind where
  parseJSON = withText "InstallerKind" $ \case
    "flatpak" -> pure Flatpak
    "brew" -> pure Brew
    "mas" -> pure Mas
    other -> fail $ "Unknown installer kind: " <> show other

instance ToJSON InstallerKind where
  toJSON = \case
    Flatpak -> "flatpak"
    Brew -> "brew"
    Mas -> "mas"

mkInstaller :: forall s es. (Driver :> es, IsSystemPackage s) => Maybe InstallerKind -> Eff es Installer
mkInstaller Nothing = osInstaller @s
mkInstaller (Just Flatpak) = pure $ flatpak @s
mkInstaller (Just Brew) = pure $ brew @s
mkInstaller (Just Mas) = pure $ mas @s

osInstaller :: forall s es. (Driver :> es, IsSystemPackage s) => Eff es Installer
osInstaller = flip fmap drvOS $ \case
  Linux (Debian _) -> apt
  Linux Fedora -> dnf
  MacOS -> brew @s
