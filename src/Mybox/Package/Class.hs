module Mybox.Package.Class (
  Package (..),
  PackageName,
  isInstalled,
  ensureInstalled,
) where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Name
import Mybox.Package.Queue.Effect
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

class
  (FromJSON a, PackageName a, Show a, ToJSON a) =>
  Package a
  where
  remoteVersion :: (Driver :> es, Stores :> es) => a -> Eff es Text
  localVersion :: (Driver :> es, Stores :> es) => a -> Eff es (Maybe Text)
  install :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => a -> Eff es ()

isInstalled :: (Driver :> es, Package a, Stores :> es) => a -> Eff es Bool
isInstalled pkg = do
  lv <- localVersion pkg
  case lv of
    Nothing -> pure False
    Just lv' -> do
      rv <- remoteVersion pkg
      pure $ lv' == rv

ensureInstalled :: (Driver :> es, InstallQueue :> es, Package a, Stores :> es, TrackerSession :> es) => a -> Eff es ()
ensureInstalled pkg = do
  installed <- isInstalled pkg
  if installed then trkSkip pkg else install pkg
