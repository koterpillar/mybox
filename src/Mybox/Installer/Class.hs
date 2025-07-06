{-# LANGUAGE UndecidableInstances #-}

module Mybox.Installer.Class (module Mybox.Installer.Class, Map) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Mybox.Aeson
import Mybox.Driver.Class
import Mybox.Prelude
import Mybox.Stores

data PackageVersion = PackageVersion
  { installed :: Maybe Text
  , latest :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON PackageVersion

instance ToJSON PackageVersion where
  toEncoding = genericToEncoding defaultOptions

class Installer i where
  iStorePackages :: i -> Store PackageVersion
  iStoreGlobal :: i -> Store Bool
  iInstall_ :: Driver :> es => i -> Text -> Eff es ()
  iUpgrade_ :: Driver :> es => i -> Text -> Eff es ()
  iGetPackageInfo :: Driver :> es => i -> Maybe Text -> Eff es (Map Text PackageVersion)

iGetCachePackageInfo :: (Driver :> es, Installer i, Stores :> es) => i -> Maybe Text -> Eff es (Map Text PackageVersion)
iGetCachePackageInfo i package = do
  results <- iGetPackageInfo i package
  for_ (Map.toList results) $ \(pkgName, pkgInfo) -> do
    storeSet (iStorePackages i) pkgName pkgInfo
  pure results

iPackageInfo :: (Driver :> es, Installer i, Stores :> es) => i -> Text -> Eff es PackageVersion
iPackageInfo i package = do
  cacheInitialized <- fromMaybe False <$> storeGet (iStoreGlobal i) ""
  unless cacheInitialized $ do
    _ <- iGetCachePackageInfo i Nothing
    storeSet (iStoreGlobal i) "" True
  storeGet (iStorePackages i) package
    >>= \case
      Just info' -> pure info'
      Nothing -> do
        info' <- iGetCachePackageInfo i (Just package)
        case Map.lookup package info' of
          Just info'' -> pure info''
          Nothing -> terror $ "Unknown package: " <> package

iInvalidate :: (Driver :> es, Installer i, Stores :> es) => i -> Text -> Eff es ()
iInvalidate = storeDelete . iStorePackages

iCombineLatestInstalled :: Map Text Text -> Map Text Text -> Map Text PackageVersion
iCombineLatestInstalled latest installed =
  Map.mapWithKey
    ( \name latestVersion ->
        PackageVersion
          { installed = Map.lookup name installed
          , latest = latestVersion
          }
    )
    latest

iInstall :: (Driver :> es, Installer i, Stores :> es) => i -> Text -> Eff es ()
iInstall i package = do
  iInstall_ i package
  iInvalidate i package

iUpgrade :: (Driver :> es, Installer i, Stores :> es) => i -> Text -> Eff es ()
iUpgrade i package = do
  iUpgrade_ i package
  iInvalidate i package

iInstalledVersion :: (Driver :> es, Installer i, Stores :> es) => i -> Text -> Eff es (Maybe Text)
iInstalledVersion i package = (.installed) <$> iPackageInfo i package

iLatestVersion :: (Driver :> es, Installer i, Stores :> es) => i -> Text -> Eff es Text
iLatestVersion i package = (.latest) <$> iPackageInfo i package
