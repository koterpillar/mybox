module Mybox.Package.System (SystemPackage (..)) where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Installer
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

data SystemPackage = SystemPackage
  { name :: Text
  , url :: Maybe Text
  , autoUpdates :: Bool
  }
  deriving (Eq, Show)

instance FromJSON SystemPackage where
  parseJSON = withObject "SystemPackage" $ \o -> do
    name <- o .: "system"
    url <- o .:? "url"
    autoUpdates <- o .:? "auto_updates" .!= True
    pure SystemPackage{..}

instance ToJSON SystemPackage where
  toJSON p =
    object
      [ "system" .= p.name
      , "url" .= p.url
      , "auto_updates" .= p.autoUpdates
      ]

autoUpdateVersion :: SystemPackage -> Text -> Text
autoUpdateVersion p = if p.autoUpdates then const "latest" else id

systemRemoteVersion :: (Driver :> es, Stores :> es) => SystemPackage -> Eff es Text
systemRemoteVersion p = case p.url of
  Nothing -> do
    installer <- mkInstaller
    autoUpdateVersion p <$> iLatestVersion installer p.name
  Just url -> drvUrlEtag url

systemLocalVersion :: (Driver :> es, Stores :> es) => SystemPackage -> Eff es (Maybe Text)
systemLocalVersion p = case p.url of
  Nothing -> do
    installer <- mkInstaller
    fmap (autoUpdateVersion p) <$> iInstalledVersion installer p.name
  Just _ -> manualVersion p

systemInstall :: (Driver :> es, Stores :> es, TrackerSession :> es) => SystemPackage -> Eff es ()
systemInstall p = do
  installer <- mkInstaller
  case p.url of
    Nothing -> do
      alreadyInstalled <- isJust <$> systemLocalVersion p
      (if alreadyInstalled then iUpgrade else iInstall) installer p.name
    Just url -> manualVersionInstall (\_ -> iInstall installer url) p

instance Package SystemPackage where
  localVersion = systemLocalVersion
  remoteVersion = systemRemoteVersion
  install = systemInstall
