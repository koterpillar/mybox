module Mybox.Package.Class (
  Package (..),
  PackageName,
  pkgIsInstalled,
) where

import Data.Aeson (FromJSON, ToJSON)

import Mybox.Driver
import Mybox.Package.Name
import Mybox.Prelude
import Mybox.Tracker

class
  (FromJSON a, PackageName a, Show a, ToJSON a) =>
  Package a
  where
  remoteVersion :: Driver :> es => a -> Eff es Text
  localVersion :: Driver :> es => a -> Eff es (Maybe Text)
  install :: (Driver :> es, TrackerSession :> es) => a -> Eff es ()

pkgIsInstalled :: (Driver :> es, Package a) => a -> Eff es Bool
pkgIsInstalled pkg = do
  lv <- localVersion pkg
  case lv of
    Nothing -> pure False
    Just lv' -> do
      rv <- remoteVersion pkg
      pure $ lv' == rv
