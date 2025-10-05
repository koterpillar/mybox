module Mybox.Package.System (SystemPackage (..), mkSystemPackage) where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Installer
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Prelude
import Mybox.Stores

data SystemPackage = SystemPackage
  { name :: Text
  , url :: Maybe Text
  , autoUpdates :: Bool
  , post :: [Text]
  }
  deriving (Eq, Show)

mkSystemPackage :: Text -> SystemPackage
mkSystemPackage name = SystemPackage{name, url = Nothing, autoUpdates = True, post = []}

instance FromJSON SystemPackage where
  parseJSON = withObjectTotal "SystemPackage" $ do
    name <- takeField "system"
    url <- takeFieldMaybe "url"
    autoUpdates <- fromMaybe True <$> takeFieldMaybe "auto_updates"
    post <- takePost
    pure SystemPackage{..}

instance ToJSON SystemPackage where
  toJSON p =
    object $
      [ "system" .= p.name
      , "url" .= p.url
      , "auto_updates" .= p.autoUpdates
      ]
        <> postToJSON p

autoUpdateVersion :: SystemPackage -> Text -> Text
autoUpdateVersion p = if p.autoUpdates then const "latest" else id

systemRemoteVersion :: (Concurrent :> es, Driver :> es, Stores :> es) => SystemPackage -> Eff es Text
systemRemoteVersion p = case p.url of
  Nothing -> do
    installer <- mkInstaller
    autoUpdateVersion p <$> iLatestVersion installer p.name
  Just url -> drvUrlEtag url

systemLocalVersion :: (Concurrent :> es, Driver :> es, Stores :> es) => SystemPackage -> Eff es (Maybe Text)
systemLocalVersion p = case p.url of
  Nothing -> do
    installer <- mkInstaller
    fmap (autoUpdateVersion p) <$> iInstalledVersion installer p.name
  Just _ -> manualVersion p

systemInstall :: App es => SystemPackage -> Eff es ()
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
  install = installWithPost systemInstall
