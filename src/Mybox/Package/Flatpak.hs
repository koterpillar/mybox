module Mybox.Package.Flatpak (FlatpakPackage (..), mkFlatpakPackage) where

import Mybox.Aeson
import Mybox.Effects
import Mybox.Installer
import Mybox.Installer.Flatpak
import Mybox.Package.Class
import Mybox.Package.Post
import Mybox.Package.Queue
import Mybox.Prelude

data FlatpakPackage = FlatpakPackage
  { name :: Text
  , post :: [Text]
  }
  deriving (Eq, Show)

mkFlatpakPackage :: Text -> FlatpakPackage
mkFlatpakPackage name = FlatpakPackage{name, post = []}

instance FromJSON FlatpakPackage where
  parseJSON = withObject "FlatpakPackage" $ \o -> do
    name <- o .: "flatpak"
    post <- parsePost o
    pure FlatpakPackage{..}

instance ToJSON FlatpakPackage where
  toJSON p =
    object $
      [ "flatpak" .= p.name
      ]
        <> postToJSON p

flatpakRemoteVersion :: App es => FlatpakPackage -> Eff es Text
flatpakRemoteVersion p = do
  queueInstall flatpakPackage
  iLatestVersion flatpak p.name

flatpakLocalVersion :: App es => FlatpakPackage -> Eff es (Maybe Text)
flatpakLocalVersion p = do
  queueInstall flatpakPackage
  iInstalledVersion flatpak p.name

flatpakInstall :: App es => FlatpakPackage -> Eff es ()
flatpakInstall p = do
  queueInstall flatpakPackage
  alreadyInstalled <- isJust <$> flatpakLocalVersion p
  (if alreadyInstalled then iUpgrade else iInstall) flatpak p.name

instance Package FlatpakPackage where
  localVersion = flatpakLocalVersion
  remoteVersion = flatpakRemoteVersion
  install = installWithPost flatpakInstall
