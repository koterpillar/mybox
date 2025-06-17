module Mybox.Package.Class
  ( Package(..)
  , PackageName(..)
  , pkgIsInstalled
  ) where

import           Mybox.Driver
import           Mybox.Package.Name
import           Mybox.Prelude
import           Mybox.Tracker

class PackageName a =>
      Package a
  where
  pkgRemoteVersion :: Driver :> es => a -> Eff es Text
  pkgLocalVersion :: Driver :> es => a -> Eff es (Maybe Text)
  pkgInstall :: (Driver :> es, Tracker :> es) => a -> Eff es ()

pkgIsInstalled :: (Package a, Driver :> es) => a -> Eff es Bool
pkgIsInstalled pkg = do
  lv <- pkgLocalVersion pkg
  case lv of
    Nothing -> pure False
    Just lv' -> do
      rv <- pkgRemoteVersion pkg
      pure $ lv' == rv
