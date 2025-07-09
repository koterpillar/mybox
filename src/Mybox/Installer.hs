module Mybox.Installer (mkInstaller) where

import Mybox.Driver
import Mybox.Installer.Apt
import Mybox.Installer.Brew
import Mybox.Installer.Class
import Mybox.Installer.DNF
import Mybox.Prelude

mkInstaller :: Driver :> es => Eff es Installer
mkInstaller =
  drvOS >>= \case
    Linux (Debian _) -> pure apt
    Linux Fedora -> pure dnf
    MacOS -> pure brew
