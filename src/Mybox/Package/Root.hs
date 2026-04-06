module Mybox.Package.Root (
  PackageRoot,
  takeRoot,
  withRoot,
) where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Prelude

type PackageRoot a = HasField "root" a Bool

takeRoot :: ObjectParser Bool
takeRoot = fromMaybe False <$> takeFieldMaybe "root"

withRoot :: (Driver :> es, PackageRoot p) => p -> Eff es a -> Eff es a
withRoot p
  | p.root = \action -> mkSudo >>= \sudo' -> modifyDriver sudo' action
  | otherwise = id
