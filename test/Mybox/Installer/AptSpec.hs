module Mybox.Installer.AptSpec where

import Mybox.Installer.Apt
import Mybox.Installer.SpecBase

spec :: Spec
spec = installerSpec Apt
