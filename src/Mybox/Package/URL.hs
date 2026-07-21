module Mybox.Package.URL (URLPackage (..), mkURLPackage) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Archive
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Prelude
import Mybox.Release

data URLPackage = URLPackage
  { url :: Text
  , archive :: ArchiveFields
  , post :: [Text]
  }
  deriving (Eq, Generic, Show)

instance FromJSON URLPackage where
  parseJSON = withObjectTotal "URLPackage" $ do
    url <- takeField "url"
    post <- takePost
    archive <- takeArchive
    pure $ URLPackage{..}

instance ToJSON URLPackage where
  toJSON p = object $ ["url" .= p.url] <> archiveToJSON p.archive <> postToJSON p

mkURLPackage :: Text -> URLPackage
mkURLPackage url =
  URLPackage
    { url = url
    , archive = emptyArchiveFields
    , post = []
    }

urlName :: Text -> Text
urlName u = domain <> "/" <> dropExtension (lastPathSegment url)
 where
  url = stripPrefix_ "https://" $ stripPrefix_ "http://" u
  domain = Text.takeWhile (/= '/') url
  lastPathSegment = Text.takeWhileEnd (/= '/')
  dropExtension = Text.takeWhile (/= '.')

instance PackageName URLPackage where
  splitName = first urlName . genericSplitName' @'[] @'["url"]

instance ArchivePackage URLPackage where
  archiveUrl p _ = pure p.url

instance Package URLPackage where
  -- FIXME: Populate Release.date from URL metadata (for example Last-Modified/created time).
  releases p = fmap mkSingleRelease $ drvUrlEtag p.url
  localVersion = manualVersion
  install = manualVersionInstall $ installWithPost archiveInstall
