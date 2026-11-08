module Mybox.Package.YumRepo (YumRepo (..), mkYumRepo) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.JSONCodec
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
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via ViaJSONCodec YumRepo

mkYumRepo :: Text -> Text -> YumRepo
mkYumRepo name url = YumRepo{name_ = name, url, gpgKey = Nothing, post = []}

instance PackageName YumRepo where
  splitName = genericSplitName' @'["yum"] @'["name_"]

instance JSONCodec YumRepo where
  jsonCodec =
    jsonCodecObject "a YUM repository" $
      field @"name_" "yum_name" "Repository name"
        :* field @"url" "yum_url" "Repository base URL"
        :* fieldOptional @"gpgKey" "gpg_key" "URL of the GPG key to import and verify packages with"
        :* fieldPost
        :* Nil

yumRepoRemoteVersion :: YumRepo -> Text
yumRepoRemoteVersion = jsonEncode

writeRepoFile :: (Driver :> es, Tracker :> es) => YumRepo -> Eff es ()
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

  let repoPath = pRoot </> "etc" </> "yum.repos.d" </> (p.name_ <> ".repo")
  sudo' <- mkSudo
  drvRun $ sudo' $ shellRaw $ "echo " <> shellQuote repoConfig <> " > " <> repoPath.text
  drvRun $ sudo' $ "chmod" :| ["a+r", repoPath.text]
  trkAdd p repoPath

importGpgKey :: Driver :> es => YumRepo -> Eff es ()
importGpgKey p = for_ p.gpgKey $ \key -> do
  sudo' <- mkSudo
  drvRun $ sudo' $ "rpmkeys" :| ["--import", key]

yumRepoInstall :: (Driver :> es, Tracker :> es) => YumRepo -> Eff es ()
yumRepoInstall p = do
  os <- drvOS
  case os of
    Linux Fedora -> do
      writeRepoFile p
      importGpgKey p
    Linux _ -> terror "YumRepo is only supported on Fedora"
    MacOS -> terror "YumRepo is only supported on Linux"

instance Package YumRepo where
  remoteVersion = pure . yumRepoRemoteVersion
  localVersion = manualVersion
  install = installWithPost $ manualVersionInstall yumRepoInstall
