{-# LANGUAGE UndecidableInstances #-}

module Mybox.Installer.Class where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Mybox.Aeson
import Mybox.Driver.Class
import Mybox.Prelude
import Mybox.Stores

class Installer i where
  iInstall :: (Driver :> es, Stores :> es) => i -> Text -> Eff es ()
  iUpgrade :: (Driver :> es, Stores :> es) => i -> Text -> Eff es ()
  iInstalledVersion :: (Driver :> es, Stores :> es) => i -> Text -> Eff es (Maybe Text)
  iLatestVersion :: (Driver :> es, Stores :> es) => i -> Text -> Eff es Text

data PackageVersion = PackageVersion
  { installed :: Maybe Text
  , latest :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON PackageVersion

instance ToJSON PackageVersion where
  toEncoding = genericToEncoding defaultOptions

class PackageCacheInstaller i where
  pciStorePackages :: i -> Store PackageVersion
  pciStoreGlobal :: i -> Store Bool
  pciInstall :: Driver :> es => i -> Text -> Eff es ()
  pciUpgrade :: Driver :> es => i -> Text -> Eff es ()
  pciGetPackageInfo :: Driver :> es => i -> Maybe Text -> Eff es (Map Text PackageVersion)

pciPackageInfo :: (Driver :> es, PackageCacheInstaller i, Stores :> es) => i -> Text -> Eff es PackageVersion
pciPackageInfo i package = do
  cacheInitialized <- fromMaybe False <$> storeGet (pciStoreGlobal i) ""
  unless cacheInitialized $ do
    allPackages <- pciGetPackageInfo i Nothing
    for_ (Map.toList allPackages) $ \(pkgName, pkgInfo) -> do
      storeSet (pciStorePackages i) pkgName pkgInfo
    storeSet (pciStoreGlobal i) "" True
  storeGet (pciStorePackages i) package
    >>= \case
      Just info' -> pure info'
      Nothing -> do
        info' <- pciGetPackageInfo i (Just package)
        case Map.lookup package info' of
          Just info'' -> do
            storeSet (pciStorePackages i) package info''
            pure info''
          Nothing -> terror $ "Unknown package: " <> package

pciInvalidate :: (Driver :> es, PackageCacheInstaller i, Stores :> es) => i -> Text -> Eff es ()
pciInvalidate = storeDelete . pciStorePackages

instance {-# OVERLAPPABLE #-} PackageCacheInstaller i => Installer i where
  iInstall i package = do
    pciInstall i package
    pciInvalidate i package
  iUpgrade i package = do
    pciUpgrade i package
    pciInvalidate i package
  iInstalledVersion i package = (.installed) <$> pciPackageInfo i package
  iLatestVersion i package = (.latest) <$> pciPackageInfo i package
