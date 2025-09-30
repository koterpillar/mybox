module Mybox.Installer (
  mkInstaller,
  iLatestVersion,
  iInstalledVersion,
  iInstall,
  iUpgrade,
) where

import Mybox.Driver
import Mybox.Installer.Apt
import Mybox.Installer.Brew
import Mybox.Installer.Class
import Mybox.Installer.DNF
import Mybox.Prelude

mkInstaller :: Driver :> es => Eff es Installer
mkInstaller = flip fmap drvOS $ \case
  Linux (Debian _) -> apt
  Linux Fedora -> dnf
  MacOS -> brew
