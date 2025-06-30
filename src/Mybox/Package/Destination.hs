module Mybox.Package.Destination (
  destinationExists,
  destinationPath,
) where

import Mybox.Driver
import Mybox.Prelude

type PackageDestination a = HasField "destination" a Text

destinationPath :: (Driver :> es, PackageDestination p) => p -> Eff es Text
destinationPath p = do
  home <- drvHome
  pure $ home </> p.destination

destinationExists :: (Driver :> es, PackageDestination p) => p -> Eff es Bool
destinationExists p = destinationPath p >>= drvIsDir
