module Mybox.Installer.DNFSpec where

import Mybox.Driver
import Mybox.Installer.DNF
import Mybox.Installer.SpecBase
import Mybox.SpecBase

spec :: Spec
spec = onlyIfOS "DNF installer tests are only available on Fedora" (\case Linux Fedora -> True; _ -> False) $ installerSpec dnf
