module Mybox.Package.Github.Internal where

import Control.Monad.Writer
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Filters
import Mybox.Package.Archive
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Prelude

data GithubPackage = GithubPackage
  { repo :: Text
  , skipReleases :: [Text]
  , archive :: ArchiveFields
  , filters :: FilterFields
  , post :: [Text]
  }
  deriving (Eq, Show)

mkGithubPackage :: Text -> GithubPackage
mkGithubPackage repo =
  GithubPackage
    { repo = repo
    , skipReleases = []
    , archive = emptyArchiveFields
    , filters = mempty
    , post = []
    }

instance HasField "name" GithubPackage Text where
  getField p = p.repo

instance FromJSON GithubPackage where
  parseJSON = withObjectTotal "GithubPackage" $ do
    repo <- takeField "repo"
    skipReleases <- takeCollapsedList "skip_release"
    archive <- takeArchive
    filters <- takeFilter
    post <- takePost
    pure GithubPackage{..}

instance ToJSON GithubPackage where
  toJSON p =
    object $
      [ "repo" .= p.repo
      , "skip_release" .= p.skipReleases
      ]
        <> archiveToJSON p.archive
        <> filterToJSON p.filters
        <> postToJSON p

api :: Driver :> es => Text -> Eff es (Either (Int, Text) Text)
api url = do
  token <- drvGithubToken
  let headers = case token of
        Just t -> ["-H", "Authorization: token " <> t]
        Nothing -> []
  (status, result) <- drvHttpGetStatusArgs headers ("https://api.github.com/" <> url)
  pure $
    if status == 200
      then Right result
      else Left (status, result)

data ReleaseArtifact = ReleaseArtifact
  { name :: Text
  , browser_download_url :: Text
  }
  deriving (Generic, Show)

instance FromJSON ReleaseArtifact

data Release = Release
  { id :: Int
  , tag_name :: Text
  , prerelease :: Bool
  , assets :: [ReleaseArtifact]
  }
  deriving (Generic, Show)

instance FromJSON Release

handleAPIError :: Either (Int, Text) a -> Eff es a
handleAPIError = \case
  Left (status, err) -> error $ "Github API returned " <> show status <> ": " <> Text.unpack err
  Right output -> pure output

releases :: Driver :> es => GithubPackage -> Eff es [Release]
releases p =
  api ("repos/" <> p.repo <> "/releases")
    >>= handleAPIError
    >>= jsonDecode "Github releases"

latestRelease :: Driver :> es => GithubPackage -> Eff es (Maybe Release)
latestRelease p =
  api ("repos/" <> p.repo <> "/releases/latest") >>= \case
    Left (404, _) -> pure Nothing
    r -> handleAPIError r >>= jsonDecode "Github release"

wantRelease :: GithubPackage -> Release -> Bool
wantRelease p r
  | r.prerelease = False
  | r.tag_name `elem` p.skipReleases = False
  | otherwise = True

release :: forall es. Driver :> es => GithubPackage -> Eff es Release
release p =
  findRelease (latestRelease p)
    `fromMaybeOrMM` findRelease (releases p)
    `fromMaybeOrMM` (error $ "No releases found for " <> Text.unpack p.repo)
 where
  findRelease :: Foldable f => Eff es (f Release) -> Eff es (Maybe Release)
  findRelease = fmap $ find $ wantRelease p

environmentFilters :: Architecture -> OS -> [Text -> Bool]
environmentFilters arch os = execWriter $ do
  tell $ map excludes_ [".asc", ".sig", "sha256", "sha512", ".yml"]
  tell $ map excludes_ [".deb", ".rpm", ".dmg", ".exe", ".appimage"]
  tell $ osFilters os
  tell $ architectureFilters arch
  case os of
    Linux _ -> do
      tell [includes_ "gnu"]
      tell [excludes_ "musl"]
    _ -> pure ()

ghFilters :: Driver :> es => GithubPackage -> Eff es [Text -> Bool]
ghFilters p = do
  arch <- drvArchitecture
  os <- drvOS
  pure $ toFilters p.filters <> environmentFilters arch os

artifact :: Driver :> es => GithubPackage -> Eff es ReleaseArtifact
artifact p = do
  r <- release p
  fs <- ghFilters p
  throwLeft $ choose_ (map (. (.name)) fs) r.assets

instance ArchivePackage GithubPackage where
  archiveUrl p = (.browser_download_url) <$> artifact p

instance Package GithubPackage where
  remoteVersion p = Text.pack . show . (.id) <$> release p
  localVersion = manualVersion
  install = manualVersionInstall $ installWithPost archiveInstall
