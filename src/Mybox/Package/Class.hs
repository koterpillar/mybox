module Mybox.Package.Class
  ( Package(..)
  , PackageName
  , pkgIsInstalled
  ) where

import           Mybox.Driver
import           Mybox.Package.Name
import           Mybox.Prelude
import           Mybox.Tracker

class PackageName a =>
      Package a
  where
  remoteVersion :: Driver :> es => a -> Eff es Text
  localVersion :: Driver :> es => a -> Eff es (Maybe Text)
  install :: (Driver :> es, PackageTracker :> es) => a -> Eff es ()

pkgIsInstalled :: (Package a, Driver :> es) => a -> Eff es Bool
pkgIsInstalled pkg = do
  lv <- localVersion pkg
  case lv of
    Nothing -> pure False
    Just lv' -> do
      rv <- remoteVersion pkg
      pure $ lv' == rv
