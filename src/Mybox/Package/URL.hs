module Mybox.Package.URL (URLPackage (..), mkURLPackage) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.JSONEx
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
  deriving (FromJSON, ToJSON) via ViaJSONEx URLPackage

instance JSONEx URLPackage where
  jsonEx =
    jsonExObject "URLPackage" $
      field @"url" "url" "TODO"
        :* fieldFlat @"archive"
        :* fieldPost
        :* Nil

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
  archiveUrl p = pure p.url

instance Package URLPackage where
  remoteVersion p = drvUrlVersion p.url
  localVersion = manualVersion
  install = manualVersionInstall $ installWithPost archiveInstall
