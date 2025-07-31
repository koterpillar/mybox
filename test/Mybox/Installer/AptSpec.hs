module Mybox.Installer.AptSpec where

import Mybox.Driver
import Mybox.Installer.Apt
import Mybox.Installer.SpecBase
import Mybox.SpecBase

spec :: Spec
spec =
  onlyIfOS (\case Linux (Debian _) -> True; _ -> False) $
    installerSpec apt
