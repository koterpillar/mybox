module Mybox.Installer.DNFSpec where

import Mybox.Driver
import Mybox.Installer.DNF
import Mybox.Installer.SpecBase
import Mybox.SpecBase

fedora :: OS -> Bool
fedora (Linux Fedora) = True
fedora _ = False

spec :: Spec
spec = onlyIf (fedora <$> drvOS) $ installerSpec dnf
