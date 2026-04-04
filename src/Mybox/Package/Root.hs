module Mybox.Package.Root (
  PackageRoot,
  takeRoot,
) where

import Mybox.Aeson
import Mybox.Prelude

type PackageRoot a = HasField "root" a Bool

takeRoot :: ObjectParser Bool
takeRoot = fromMaybe False <$> takeFieldMaybe "root"
