module Mybox.Package.Root (
  PackageRoot,
  fieldRoot,
  withRoot,
) where

import Mybox.Driver
import Mybox.JSONEx
import Mybox.Prelude

type PackageRoot a = HasField "root" a Bool

fieldRoot :: Field "root" Bool
fieldRoot = fieldDefault "root" "TODO" False

withRoot :: (Driver :> es, PackageRoot p) => p -> Eff es a -> Eff es a
withRoot p
  | p.root = \action -> mkSudo >>= \sudo' -> modifyDriver sudo' action
  | otherwise = id
