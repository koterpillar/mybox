module GitHub where

import           Data.Aeson

import           Data.Text           (Text)
import qualified Data.Text           as Text

import           GHC.Generics

import           Network.HTTP.Simple

data GitHubRepo =
  GitHubRepo
    { grOrganisation :: Text
    , grRepository   :: Text
    }
  deriving (Eq, Show)

grString :: GitHubRepo -> Text
grString GitHubRepo {..} = grOrganisation <> "/" <> grRepository

instance FromJSON GitHubRepo where
  parseJSON v =
    parseJSON @Text v >>= \t ->
      case Text.splitOn "/" t of
        [o, r] -> pure $ GitHubRepo o r
        _      -> fail "Expected organisation and repository"

grLatestRelease :: GitHubRepo -> IO GitHubRelease
grLatestRelease repo = do
  request <-
    parseRequestThrow $
    Text.unpack $
    "https://api.github.com/repos/" <> grString repo <> "/releases/latest"
  getResponseBody <$> httpJSON request

newtype GitHubRelease =
  GitHubRelease
    { grrAssets :: [GitHubReleaseAsset]
    }
  deriving (Show, Generic)

instance FromJSON GitHubRelease where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

data GitHubReleaseAsset =
  GitHubReleaseAsset
    { grraName               :: Text
    , grraBrowserDownloadURL :: Text
    }
  deriving (Show, Generic)

instance FromJSON GitHubReleaseAsset where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
