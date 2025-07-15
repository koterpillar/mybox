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

data Installer = Installer
  { storePackages :: Store PackageVersion
  , storeGlobal :: Store Bool
  , install_ :: forall es. Driver :> es => Text -> Eff es ()
  , upgrade_ :: forall es. Driver :> es => Text -> Eff es ()
  , getPackageInfo :: forall es. Driver :> es => Maybe Text -> Eff es (Map Text PackageVersion)
  }

iGetCachePackageInfo :: (Driver :> es, Stores :> es) => Installer -> Maybe Text -> Eff es (Map Text PackageVersion)
iGetCachePackageInfo i package = do
  results <- getPackageInfo i package
  for_ (Map.toList results) $ \(pkgName, pkgInfo) -> do
    storeSet i.storePackages pkgName pkgInfo
  pure results

iPackageInfo :: (Driver :> es, Stores :> es) => Installer -> Text -> Eff es PackageVersion
iPackageInfo i package = do
  cacheInitialized <- fromMaybe False <$> storeGet i.storeGlobal ""
  unless cacheInitialized $ do
    _ <- iGetCachePackageInfo i Nothing
    storeSet i.storeGlobal "" True
  storeGet i.storePackages package
    >>= \case
      Just info' -> pure info'
      Nothing -> do
        info' <- iGetCachePackageInfo i (Just package)
        case Map.lookup package info' of
          Just info'' -> pure info''
          Nothing -> terror $ "Unknown package: " <> package

iInvalidate :: (Driver :> es, Stores :> es) => Installer -> Text -> Eff es ()
iInvalidate = storeDelete . storePackages

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

iInstall :: (Driver :> es, Stores :> es) => Installer -> Text -> Eff es ()
iInstall i package = do
  install_ i package
  iInvalidate i package

iUpgrade :: (Driver :> es, Stores :> es) => Installer -> Text -> Eff es ()
iUpgrade i package = do
  upgrade_ i package
  iInvalidate i package

iInstalledVersion :: (Driver :> es, Stores :> es) => Installer -> Text -> Eff es (Maybe Text)
iInstalledVersion i package = (.installed) <$> iPackageInfo i package

iLatestVersion :: (Driver :> es, Stores :> es) => Installer -> Text -> Eff es Text
iLatestVersion i package = (.latest) <$> iPackageInfo i package
