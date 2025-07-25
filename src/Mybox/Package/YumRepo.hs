module Mybox.Package.YumRepo (YumRepo (..), mkYumRepo) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Prelude
import Mybox.Tracker

data YumRepo = YumRepo
  { name_ :: Text
  , url :: Text
  , gpgKey :: Maybe Text
  , post :: [Text]
  }
  deriving (Eq, Show)

mkYumRepo :: Text -> Text -> YumRepo
mkYumRepo name url = YumRepo{name_ = name, url, gpgKey = Nothing, post = []}

instance HasField "name" YumRepo Text where
  getField p = "yum-" <> p.name_

instance FromJSON YumRepo where
  parseJSON = withObject "YumRepo" $ \o -> do
    name_ <- o .: "yum_name"
    url <- o .: "yum_url"
    gpgKey <- o .:? "gpg_key"
    post <- parsePost o
    pure YumRepo{..}

instance ToJSON YumRepo where
  toJSON p =
    object $
      [ "yum_name" .= p.name_
      , "yum_url" .= p.url
      , "gpg_key" .= p.gpgKey
      ]
        <> postToJSON p

yumRepoRemoteVersion :: YumRepo -> Text
yumRepoRemoteVersion = jsonEncode

writeRepoFile :: (Driver :> es, TrackerSession :> es) => YumRepo -> Eff es ()
writeRepoFile p = do
  let repoConfig =
        Text.unlines $
          [ "[" <> p.name_ <> "]"
          , "name=" <> p.name_
          , "baseurl=" <> p.url
          , "enabled=1"
          ]
            ++ ( toList p.gpgKey
                   >>= \key ->
                     [ "gpgcheck=1"
                     , "gpgkey=" <> key
                     ]
               )

  let repoPath = "/etc" </> "yum.repos.d" </> p.name_ <> ".repo"
  drvRun $ sudo $ shellRaw $ "echo " <> shellQuote repoConfig <> " > " <> repoPath
  drvRun $ sudo $ "chmod" :| ["a+r", repoPath]
  trkAdd p repoPath

importGpgKey :: Driver :> es => YumRepo -> Eff es ()
importGpgKey p = for_ p.gpgKey $ \key ->
  drvRun $ sudo $ "rpm" :| ["--import", key]

yumRepoInstall :: (Driver :> es, TrackerSession :> es) => YumRepo -> Eff es ()
yumRepoInstall p = do
  os <- drvOS
  case os of
    Linux Fedora -> do
      writeRepoFile p
      importGpgKey p
    Linux (Debian _) -> terror "YumRepo is only supported on Fedora"
    MacOS -> terror "YumRepo is only supported on Linux"

instance Package YumRepo where
  remoteVersion = pure . yumRepoRemoteVersion
  localVersion = manualVersion
  install = installWithPost $ manualVersionInstall yumRepoInstall
