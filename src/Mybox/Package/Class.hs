module Mybox.Package.Class (
  Package (..),
  PackageName,
  pathname,
  isInstalled,
  ensureInstalled,
) where

import Mybox.Aeson
import Mybox.Effects
import Mybox.Package.Name
import Mybox.Prelude
import Mybox.Tracker

class
  (FromJSON a, PackageName a, Show a, ToJSON a) =>
  Package a
  where
  remoteVersion :: App es => a -> Eff es Text
  localVersion :: App es => a -> Eff es (Maybe Text)
  install :: App es => a -> Eff es ()

isInstalled :: (App es, Package a) => a -> Eff es Bool
isInstalled pkg = do
  lv <- localVersion pkg
  case lv of
    Nothing -> pure False
    Just lv' -> do
      rv <- remoteVersion pkg
      pure $ lv' == rv

ensureInstalled :: (App es, Package a) => a -> Eff es ()
ensureInstalled pkg = do
  installed <- isInstalled pkg
  if installed then trkSkip pkg else install pkg
