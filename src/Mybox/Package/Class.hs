module Mybox.Package.Class where

import           Mybox.Driver
import           Mybox.Prelude

class Package a where
  pkgName :: a -> Text
  pkgRemoteVersion :: Driver :> es => a -> Eff es Text
  pkgLocalVersion :: Driver :> es => a -> Eff es (Maybe Text)
  pkgInstall :: Driver :> es => a -> Eff es ()

pkgIsInstalled :: (Package a, Driver :> es) => a -> Eff es Bool
pkgIsInstalled pkg = do
  lv <- pkgLocalVersion pkg
  case lv of
    Nothing -> pure False
    Just lv' -> do
      rv <- pkgRemoteVersion pkg
      pure $ lv' == rv
