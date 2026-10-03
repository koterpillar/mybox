{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Package.System.Class where

import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Prelude

-- System package depends on installers. To avoid circular dependencies,
-- define a class for it.

class Package s => IsSystemPackage s where
  mkSystemPackage_ :: Text -> [Text] -> s

ensureGit_ :: forall s es. (App es, IsSystemPackage s) => Eff es ()
ensureGit_ = unlessExecutableExists "git" $ queueInstall $ mkSystemPackage_ @s "git" []
