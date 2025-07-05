{-# LANGUAGE UndecidableInstances #-}

module Mybox.Installer.Class where

import Mybox.Driver.Class
import Mybox.Prelude
import Mybox.Stores

class Installer i where
  iInstall :: (Driver :> es, Stores :> es) => i -> Text -> Eff es ()
  iUpgrade :: (Driver :> es, Stores :> es) => i -> Text -> Eff es ()
  iInstalledVersion :: (Driver :> es, Stores :> es) => i -> Text -> Eff es (Maybe Text)
  iLatestVersion :: (Driver :> es, Stores :> es) => i -> Text -> Eff es Text
