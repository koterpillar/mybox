module Mybox.Package.Forge.Internal where

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

data ForgePackage = ForgePackage
  { repo :: Text
  , skipReleases :: [Text]
  , archive :: ArchiveFields
  , filters :: FilterFields
  , post :: [Text]
  }
  deriving (Eq, Generic, Show)

mkForgePackage :: Text -> ForgePackage
mkForgePackage repo =
  ForgePackage
    { repo = repo
    , skipReleases = []
    , archive = emptyArchiveFields
    , filters = mempty
    , post = []
    }

instance PackageName ForgePackage where
  splitName = genericSplitName' @'[] @'["repo"]

instance FromJSON ForgePackage where
  parseJSON = withObjectTotal "ForgePackage" $ do
    repo <- takeField "repo"
    skipReleases <- takeCollapsedList "skip_release"
    archive <- takeArchive
    filters <- takeFilter
    post <- takePost
    pure ForgePackage{..}

instance ToJSON ForgePackage where
  toJSON p =
    object $
      [ "repo" .= p.repo
      , "skip_release" .= p.skipReleases
      ]
        <> archiveToJSON p.archive
        <> filterToJSON p.filters
        <> postToJSON p

data APIEndpoint = APIEndpoint
  { baseUrl :: Text
  , authToken :: Maybe Text
  }

apiEndpoint :: Driver :> es => ForgePackage -> Eff es APIEndpoint
apiEndpoint p = do
  let parts = Text.splitOn "/" p.repo
  case parts of
    [owner, repo] -> githubApiEndpoint owner repo
    [host, owner, repo] -> genericApiEndpoint host owner repo
    [scheme, "", host, owner, repo] -> genericApiEndpoint (scheme <> "//" <> host) owner repo
    _ -> error $ "Invalid repo format: " <> Text.unpack p.repo

genericApiEndpoint :: Driver :> es => Text -> Text -> Text -> Eff es APIEndpoint
genericApiEndpoint host owner repo = do
  -- https://codeberg.org/api/swagger
  -- https://gitea.com/api/swagger
  -- API is compatible with GitHub, but the prefix is different
  let authToken = Nothing
  let start = if Text.isInfixOf ":" host then host else "https://" <> host
  let baseUrl = start <> "/api/v1/repos/" <> owner <> "/" <> repo
  pure APIEndpoint{..}

githubApiEndpoint :: Driver :> es => Text -> Text -> Eff es APIEndpoint
githubApiEndpoint owner repo = do
  authToken <- drvGithubToken
  let baseUrl = "https://api.github.com/repos/" <> owner <> "/" <> repo
  pure APIEndpoint{..}

api :: Driver :> es => ForgePackage -> Text -> Eff es (Either (Int, Text) Text)
api p url = do
  endpoint <- apiEndpoint p
  let headers = case endpoint.authToken of
        Just t -> ["-H", "Authorization: token " <> t]
        Nothing -> []
  (status, result) <- drvHttpGetStatusArgs headers (endpoint.baseUrl <> url)
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
  Left (status, err) -> error $ "Releases API returned " <> show status <> ": " <> Text.unpack err
  Right output -> pure output

releases :: Driver :> es => ForgePackage -> Eff es [Release]
releases p =
  api p "/releases"
    >>= handleAPIError
    >>= jsonDecode "Releases"

latestRelease :: Driver :> es => ForgePackage -> Eff es (Maybe Release)
latestRelease p =
  api p "/releases/latest" >>= \case
    Left (404, _) -> pure Nothing
    r -> handleAPIError r >>= jsonDecode "Release"

wantRelease :: ForgePackage -> Release -> Bool
wantRelease p r
  | r.prerelease = False
  | r.tag_name `elem` p.skipReleases = False
  | otherwise = True

release :: forall es. Driver :> es => ForgePackage -> Eff es Release
release p =
  findRelease (latestRelease p)
    `fromMaybeOrMM` findRelease (releases p)
    `fromMaybeOrMM` (error $ "No releases found for " <> Text.unpack p.repo)
 where
  findRelease :: Foldable f => Eff es (f Release) -> Eff es (Maybe Release)
  findRelease = fmap $ find $ wantRelease p

environmentFilters :: Architecture -> OS -> [Text -> Bool]
environmentFilters arch os = execWriter $ do
  tell $ map excludes_ [".asc", ".sig", "sha256", "sha512", "sbom", ".yml"]
  tell $ map excludes_ [".deb", ".rpm", ".dmg", ".exe", ".appimage"]
  tell $ osFilters os
  tell $ architectureFilters arch
  case os of
    Linux (Generic "alpine") -> tell [includes_ "musl"]
    Linux _ -> do
      tell [includes_ "gnu"]
      tell [excludes_ "musl"]
    _ -> pure ()

ghFilters :: Driver :> es => ForgePackage -> Eff es [Text -> Bool]
ghFilters p = do
  arch <- drvArchitecture
  os <- drvOS
  pure $ toFilters p.filters <> environmentFilters arch os

artifact :: Driver :> es => ForgePackage -> Eff es ReleaseArtifact
artifact p = do
  r <- release p
  fs <- ghFilters p
  throwLeft $ choose_ (map (. (.name)) fs) r.assets

instance ArchivePackage ForgePackage where
  archiveUrl p = (.browser_download_url) <$> artifact p

instance Package ForgePackage where
  remoteVersion p = Text.pack . show . (.id) <$> release p
  localVersion = manualVersion
  install = manualVersionInstall $ installWithPost archiveInstall
