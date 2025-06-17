module Mybox.Package.Name where

import           Mybox.Prelude

class PackageName a where
  pkgName :: a -> Text
