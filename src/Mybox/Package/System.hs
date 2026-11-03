module Mybox.Package.System (
  SystemPackage (..),
  InstallerKind (..),
  mkSystemPackage,
  ensureGit,
) where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Installer
import Mybox.JSONCodec
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Package.System.Class
import Mybox.Prelude

data SystemPackage = SystemPackage
  { name :: Text
  , installer :: Maybe InstallerKind
  , url :: Maybe Text
  , autoUpdates :: Bool
  , post :: [Text]
  }
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via ViaJSONCodec SystemPackage

instance PackageName SystemPackage where
  splitName = genericSplitName

mkSystemPackage :: Text -> SystemPackage
mkSystemPackage name = SystemPackage{name, installer = Nothing, url = Nothing, autoUpdates = True, post = []}

instance IsSystemPackage SystemPackage where
  mkSystemPackage_ name post = (mkSystemPackage name){post}

ensureGit :: App es => Eff es ()
ensureGit = ensureGit_ @SystemPackage

instance JSONCodec SystemPackage where
  jsonCodec =
    jsonCodecObject "SystemPackage" $
      field @"name" "system" "Package name for the system package manager"
        :* fieldOptional @"installer" "installer" "Use this installer instead of the system package manager"
        :* fieldOptional @"url" "url" "Install from this package file URL instead of the repositories"
        :* fieldDefault @"autoUpdates" "auto_updates" "Assume package upgrades automatically, only install if missing"
        :* fieldPost
        :* Nil

autoUpdateVersion :: SystemPackage -> Text -> Text
autoUpdateVersion p = if p.autoUpdates then const "latest" else id

systemInstaller :: Driver :> es => SystemPackage -> Eff es Installer
systemInstaller p = mkInstaller @SystemPackage p.installer

systemRemoteVersion :: App es => SystemPackage -> Eff es Text
systemRemoteVersion p = case p.url of
  Nothing -> do
    installer <- systemInstaller p
    autoUpdateVersion p <$> iLatestVersion installer p.name
  Just url -> drvUrlVersion url

systemLocalVersion :: App es => SystemPackage -> Eff es (Maybe Text)
systemLocalVersion p = case p.url of
  Nothing -> do
    installer <- systemInstaller p
    fmap (autoUpdateVersion p) <$> iInstalledVersion installer p.name
  Just _ -> manualVersion p

systemInstall :: App es => SystemPackage -> Eff es ()
systemInstall p = do
  installer <- systemInstaller p
  case p.url of
    Nothing -> do
      alreadyInstalled <- isJust <$> systemLocalVersion p
      (if alreadyInstalled then iUpgrade else iInstall) installer p.name
    Just url -> manualVersionInstall (\_ -> iInstallURL installer url) p

instance Package SystemPackage where
  localVersion = systemLocalVersion
  remoteVersion = systemRemoteVersion
  install = installWithPost systemInstall
