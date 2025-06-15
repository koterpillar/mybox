module Mybox.Package.Class where

import           Mybox.Driver
import           Mybox.Prelude

class Package a where
  pkgName :: a -> Text
  pkgRemoteVersion :: MonadDriver m => a -> m Text
  pkgLocalVersion :: MonadDriver m => a -> m (Maybe Text)
  pkgInstall :: MonadDriver m => a -> m ()

pkgIsInstalled :: (Package a, MonadDriver m) => a -> m Bool
pkgIsInstalled pkg = do
  lv <- pkgLocalVersion pkg
  case lv of
    Nothing -> pure False
    Just lv' -> do
      rv <- pkgRemoteVersion pkg
      pure $ lv' == rv
