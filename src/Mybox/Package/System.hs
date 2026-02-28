module Mybox.Package.System (SystemPackage (..), InstallerKind (..), mkSystemPackage) where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Installer
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Prelude

data SystemPackage = SystemPackage
  { name :: Text
  , installer :: Maybe InstallerKind
  , url :: Maybe Text
  , autoUpdates :: Bool
  , post :: [Text]
  }
  deriving (Eq, Generic, Show)

instance PackageName SystemPackage where
  withoutName = genericWithoutName

mkSystemPackage :: Text -> SystemPackage
mkSystemPackage name = SystemPackage{name, installer = Nothing, url = Nothing, autoUpdates = True, post = []}

instance IsSystemPackage SystemPackage where
  mkSystemPackage_ name post = (mkSystemPackage name){post}

instance FromJSON SystemPackage where
  parseJSON = withObjectTotal "SystemPackage" $ do
    name <- takeField "system"
    installer <- takeFieldMaybe "installer"
    url <- takeFieldMaybe "url"
    autoUpdates <- fromMaybe False <$> takeFieldMaybe "auto_updates"
    post <- takePost
    pure SystemPackage{..}

instance ToJSON SystemPackage where
  toJSON p =
    object $
      [ "system" .= p.name
      , "installer" .= p.installer
      , "url" .= p.url
      , "auto_updates" .= p.autoUpdates
      ]
        <> postToJSON p

autoUpdateVersion :: SystemPackage -> Text -> Text
autoUpdateVersion p = if p.autoUpdates then const "latest" else id

systemInstaller :: Driver :> es => SystemPackage -> Eff es Installer
systemInstaller p = mkInstaller @SystemPackage p.installer

systemRemoteVersion :: App es => SystemPackage -> Eff es Text
systemRemoteVersion p = case p.url of
  Nothing -> do
    installer <- systemInstaller p
    autoUpdateVersion p <$> iLatestVersion installer p.name
  Just url -> drvUrlEtag url

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
