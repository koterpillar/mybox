module Mybox.Package.Github (GithubPackage (..), mkGithubPackage) where

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
  , raw :: Either Text Bool
  , binaries :: [Text]
  , binaryWrapper :: Bool
  , binaryPaths :: [Path Rel]
  , apps :: [Text]
  , fonts :: [Text]
  , prefixes :: [Text]
  , suffixes :: [Text]
  , includes :: [Text]
  , excludes :: [Text]
  , post :: [Text]
  }
  deriving (Eq, Show)

mkGithubPackage :: Text -> GithubPackage
mkGithubPackage repo =
  GithubPackage
    { repo = repo
    , skipReleases = []
    , raw = Right False
    , binaries = []
    , binaryWrapper = False
    , binaryPaths = []
    , apps = []
    , fonts = []
    , prefixes = []
    , suffixes = []
    , includes = []
    , excludes = []
    , post = []
    }

instance HasField "name" GithubPackage Text where
  getField p = p.repo

instance FromJSON GithubPackage where
  parseJSON = withObject "GithubPackage" $ \o -> do
    repo <- o .: "repo"
    skipReleases <- parseCollapsedList o "skip_release"
    ArchiveFields{..} <- parseArchive o
    FilterFields{..} <- parseFilter o
    post <- parsePost o
    pure GithubPackage{..}

instance ToJSON GithubPackage where
  toJSON p =
    object $
      [ "repo" .= p.repo
      , "skip_release" .= p.skipReleases
      ]
        <> archiveToJSON p
        <> filterToJSON p
        <> postToJSON p

api :: Driver :> es => Text -> Eff es (Either Text Text)
api url = do
  token <- drvGithubToken
  result <-
    drvRunOutputExit $
      "curl"
        :| [ "-H"
           , "Authorization: token " <> token
           , "-fsSL"
           , "https://api.github.com/" <> url
           ]
  pure $
    if result.exit == ExitSuccess
      then Right result.output
      else Left result.output

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

releases :: Driver :> es => GithubPackage -> Eff es [Release]
releases p =
  api ("repos/" <> p.repo <> "/releases")
    >>= either terror pure
    >>= jsonDecode "Github releases"

latestRelease :: Driver :> es => GithubPackage -> Eff es (Maybe Release)
latestRelease p =
  api ("repos/" <> p.repo <> "/releases/latest") >>= \case
    Left err
      | Text.isInfixOf "The requested URL returned error: 404" err -> pure Nothing
      | otherwise -> terror err
    Right output -> jsonDecode "Github release" output

wantRelease :: GithubPackage -> Release -> Bool
wantRelease p r
  | r.prerelease = False
  | r.tag_name `elem` p.skipReleases = False
  | otherwise = True

release :: forall es. Driver :> es => GithubPackage -> Eff es Release
release p =
  findRelease (latestRelease p)
    `fromMaybeOrMM` findRelease (releases p)
    `fromMaybeOrMM` terror ("No releases found for " <> p.repo)
 where
  findRelease :: Foldable f => Eff es (f Release) -> Eff es (Maybe Release)
  findRelease = fmap $ find $ wantRelease p

environmentFilters :: Architecture -> OS -> [Text -> Bool]
environmentFilters arch os = execWriter $ do
  let tell1 = tell . pure
  for_ [".asc", ".sig", "sha256", "sha512", ".yml"] $ \hint -> do
    tell1 $ excludes_ hint
  for_ [".deb", ".rpm", ".dmg", ".exe", ".appimage"] $ \hint -> do
    tell1 $ excludes_ hint
  tell $ osFilters os
  tell $ architectureFilters arch
  case os of
    Linux _ -> do
      tell1 $ includes_ "gnu"
      tell1 $ excludes_ "musl"
    _ -> pure ()

ghFilters :: Driver :> es => GithubPackage -> Eff es [Text -> Bool]
ghFilters p = do
  arch <- drvArchitecture
  os <- drvOS
  pure $ filters p <> environmentFilters arch os

artifact :: Driver :> es => GithubPackage -> Eff es ReleaseArtifact
artifact p = do
  r <- release p
  fs <- ghFilters p
  pure $ choose_ (map (. (.name)) fs) r.assets

instance ArchivePackage GithubPackage where
  archiveUrl p = (.browser_download_url) <$> artifact p

instance Package GithubPackage where
  remoteVersion p = Text.pack . show . (.id) <$> release p
  localVersion = manualVersion
  install = manualVersionInstall $ installWithPost archiveInstall
