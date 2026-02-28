module Mybox.Package.URL (URLPackage (..), mkURLPackage) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Archive
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Prelude

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

urlName :: URLPackage -> Text
urlName p = domain <> "/" <> dropExtension (lastPathSegment url)
 where
  url = maybeModify (Text.stripPrefix "https://") $ maybeModify (Text.stripPrefix "http://") $ p.url
  domain = Text.takeWhile (/= '/') url
  lastPathSegment = Text.takeWhileEnd (/= '/')
  dropExtension = Text.takeWhile (/= '.')

maybeModify :: (a -> Maybe a) -> a -> a
maybeModify f x = fromMaybe x $ f x

instance HasField "name" URLPackage Text where
  getField = urlName

instance PackageName URLPackage where
  withoutName = genericWithoutName' ["url"]

instance ArchivePackage URLPackage where
  archiveUrl p = pure p.url

instance Package URLPackage where
  remoteVersion p = drvUrlEtag p.url
  localVersion = manualVersion
  install = manualVersionInstall $ installWithPost archiveInstall
