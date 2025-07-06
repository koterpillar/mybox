module Mybox.Installer.AptSpec where

import Mybox.Driver
import Mybox.Installer.Apt
import Mybox.Installer.SpecBase
import Mybox.SpecBase

debianLike :: OS -> Bool
debianLike (Linux "debian") = True
debianLike (Linux "ubuntu") = True
debianLike _ = False

spec :: Spec
spec = onlyIf (debianLike <$> drvOS) $ installerSpec Apt
