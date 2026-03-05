module Mybox.Package.Class (
  Package (..),
  PackageName (..),
  getName,
  withoutName,
  genericSplitName,
  genericSplitName',
  pathname,
  isInstalled,
  ensureInstalled,
) where

import Mybox.Aeson
import Mybox.Display
import Mybox.Effects
import Mybox.Package.Hash
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
  lh <- localHash pkg
  if lh /= pkgHash pkg
    then pure False
    else do
      lv <- localVersion pkg
      case lv of
        Nothing -> pure False
        Just lv' -> do
          rv <- remoteVersion pkg
          pure $ lv' == rv

ensureInstalled :: (App es, Package a) => a -> Eff es ()
ensureInstalled pkg = trkTry pkg $
  flip withException (displayBanner . bannerFailed (getName pkg)) $ do
    installed <- displayBannerWhile (bannerChecking $ getName pkg) $ isInstalled pkg
    if installed
      then do
        displayBanner $ bannerUnchanged $ getName pkg
        trkSkip pkg
      else do
        displayBannerWhile (bannerInstalling $ getName pkg) $ do
          install pkg
          writeHash pkg
        displayBanner $ bannerModified $ getName pkg
