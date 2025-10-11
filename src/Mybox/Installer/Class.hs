module Mybox.Installer.Class where

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
  { storeKey :: Text
  , install_ :: forall es. Driver :> es => Text -> Eff es ()
  , upgrade_ :: forall es. Driver :> es => Text -> Eff es ()
  , getPackageInfo :: forall es. Driver :> es => Maybe Text -> Eff es (Map Text PackageVersion)
  }

instance HasField "storePackages" Installer (Store (Map Text PackageVersion)) where
  getField i = Store{key = "installer-" <> i.storeKey <> "-packages", iso = jsonIso, def = Map.empty}

iLocked :: (Concurrent :> es, Stores :> es) => Installer -> Eff es a -> Eff es a
iLocked i = storeLocked $ Store{key = "installer-" <> i.storeKey <> "-lock", iso = jsonIso, def = ()}

iGetCachePackageInfo :: (Concurrent :> es, Driver :> es, Stores :> es) => Installer -> Maybe Text -> Eff es (Map Text PackageVersion)
iGetCachePackageInfo i package = do
  results <- iLocked i $ getPackageInfo i package
  storeModify i.storePackages $ Map.union results
  pure results

iInitLocked :: (Concurrent :> es, Stores :> es) => Installer -> Eff es () -> Eff es ()
iInitLocked i = storeInitLocked $ Store{key = "installer-" <> i.storeKey <> "-global", iso = jsonIso, def = Nothing}

iPackageInfo :: (Concurrent :> es, Driver :> es, Stores :> es) => Installer -> Text -> Eff es PackageVersion
iPackageInfo i package = do
  iInitLocked i $ void $ iGetCachePackageInfo i Nothing
  (Map.lookup package <$> storeGet i.storePackages)
    `fromMaybeOrMM` fmap (Map.lookup package) (iGetCachePackageInfo i $ Just package)
    `fromMaybeOrMM` terror ("Unknown package: " <> package)

iInvalidate :: (Concurrent :> es, Driver :> es, Stores :> es) => Installer -> Text -> Eff es ()
iInvalidate i = storeModify i.storePackages . Map.delete

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

iInstall :: (Concurrent :> es, Driver :> es, Stores :> es) => Installer -> Text -> Eff es ()
iInstall i package = do
  install_ i package
  iInvalidate i package

iUpgrade :: (Concurrent :> es, Driver :> es, Stores :> es) => Installer -> Text -> Eff es ()
iUpgrade i package = do
  upgrade_ i package
  iInvalidate i package

iInstalledVersion :: (Concurrent :> es, Driver :> es, Stores :> es) => Installer -> Text -> Eff es (Maybe Text)
iInstalledVersion i package = (.installed) <$> iPackageInfo i package

iLatestVersion :: (Concurrent :> es, Driver :> es, Stores :> es) => Installer -> Text -> Eff es Text
iLatestVersion i package = (.latest) <$> iPackageInfo i package
