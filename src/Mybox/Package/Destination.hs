module Mybox.Package.Destination (
  destinationExists,
  destinationPath,
) where

import Mybox.Driver
import Mybox.Package.Root
import Mybox.Prelude

type PackageDestination a = (HasField "destination" a (Path AnyAnchor), PackageRoot a)

destinationPath :: (Driver :> es, PackageDestination p) => p -> Eff es (Path Abs)
destinationPath p = do
  home <- drvHome_ $ if p.root then "root" else ""
  pure $ home <//> p.destination

destinationExists :: (Driver :> es, PackageDestination p) => p -> Eff es Bool
destinationExists p = destinationPath p >>= drvIsDir
