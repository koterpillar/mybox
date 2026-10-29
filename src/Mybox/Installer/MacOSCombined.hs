{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Installer.MacOSCombined (macOSCombined) where

import Mybox.Installer.Brew
import Mybox.Installer.Class
import Mybox.Installer.MacOS
import Mybox.Package.System.Class

-- | Install packages by name with Brew, and from URLs with the macOS installer.
macOSCombined :: forall s. IsSystemPackage s => Installer
macOSCombined =
  Installer
    { storeKey = storeKey (brew @s)
    , install_ = install_ (brew @s)
    , installURL = installURL macOS
    , upgrade_ = upgrade_ (brew @s)
    , getPackageInfo = getPackageInfo (brew @s)
    }
