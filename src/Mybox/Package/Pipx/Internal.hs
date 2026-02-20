module Mybox.Package.Pipx.Internal where

import Data.Char (isAlphaNum)
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Archive
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Package.Queue
import Mybox.Package.Release hiding (repo)
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Tracker

data PipxPackage = PipxPackage
  { package :: Text
  , post :: [Text]
  }
  deriving (Eq, Show)

mkPipxPackage :: Text -> PipxPackage
mkPipxPackage package = PipxPackage{package, post = []}

instance HasField "name" PipxPackage Text where
  getField p = Text.toLower p.package

instance FromJSON PipxPackage where
  parseJSON = withObjectTotal "PipxPackage" $ do
    package <- takeField "pipx"
    post <- takePost
    pure PipxPackage{..}

instance ToJSON PipxPackage where
  toJSON p = object $ ["pipx" .= p.package] <> postToJSON p

repo :: PipxPackage -> Maybe (Text, Maybe Text)
repo p = do
  url <- Text.stripPrefix "git+" p.package
  case Text.splitOn "@" url of
    [_] -> Just (url, Nothing)
    [p1, p2, p3] -> Just (p1 <> "@" <> p2, Just p3)
    [p1, p2]
      | isSchemeUser p1 -> Just (url, Nothing)
      | otherwise -> Just (p1, Just p2)
    _ -> Nothing
 where
  isSchemeUser = all (Text.all isAlphaNum) . Text.splitOn "://"

isRepo :: PipxPackage -> Bool
isRepo = isJust . repo

newtype PipxList = PipxList {packages :: [PipxInstalledPackage]} deriving (Show)

instance FromJSON PipxList where
  parseJSON = withObject "PipxList" $ \obj -> do
    packages <- obj .: "venvs"
    pure $ PipxList{packages = toList (packages :: Map Text PipxInstalledPackage)}

prerequisites :: App es => PipxPackage -> Eff es ()
prerequisites p = do
  os <- drvOS
  let packages = case os of
        Linux distribution ->
          ["python3-pip"] <> case distribution of
            Debian _ -> ["python3-venv"]
            _ -> []
        MacOS -> []
  for_ packages $ \package ->
    queueInstall $ mkSystemPackage package
  queueInstall $
    (mkReleasePackage "pypa/pipx")
      { archive = emptyArchiveFields{binaries = ["pipx"], raw = Left "pipx"}
      }
  when (isRepo p) $
    queueInstall $
      mkSystemPackage "git"

pipx :: (Concurrent :> es, Driver :> es) => (Args -> Eff es a) -> [Text] -> Eff es a
pipx run args = drvAtomic "pipx" $ run $ "pipx" :| args

data PipxInstalledPackage = PipxInstalledPackage
  { name :: Text
  , canonicalName :: Text
  , version :: Maybe Text
  , binaries :: [Path Abs]
  }
  deriving (Show)

instance FromJSON PipxInstalledPackage where
  parseJSON = withObject "PipxInstalledPackage" $ \obj -> do
    metadata <- obj .: "metadata"
    mainPackage <- metadata .: "main_package"
    name <- mainPackage .: "package_or_url"
    canonicalName <- mainPackage .: "package"
    version <- mainPackage .:? "package_version"
    binaries <- mainPackage .: "app_paths" >>= traverse (.: "__Path__")
    pure $ PipxInstalledPackage{..}

getInstalled :: (Concurrent :> es, Driver :> es) => PipxPackage -> Eff es (Maybe PipxInstalledPackage)
getInstalled p = do
  pipxListOutput <- pipx drvRunOutput ["list", "--json"]
  allMetadata <- jsonDecode @PipxList "pipx output" pipxListOutput
  pure $ find (\pkg -> pkg.name == p.package) allMetadata.packages

localVersionPipx :: (Concurrent :> es, Driver :> es, Tracker :> es) => PipxPackage -> Eff es (Maybe Text)
localVersionPipx p = do
  if isRepo p
    then manualVersion p -- pipx doesn't store Git commit, just the version field from the package
    else do
      metadata <- getInstalled p
      pure $ metadata >>= (.version)

remoteVersionPipx :: App es => PipxPackage -> Eff es Text
remoteVersionPipx p = do
  prerequisites p
  case repo p of
    Just (r, b) -> drvRepoBranchVersion r b
    Nothing -> do
      result <-
        drvRunOutput $
          "python3" :| ["-m", "pip", "index", "versions", p.package]
      maybe (terror "Cannot parse pip output") pure $ do
        versionLine <- listToMaybe $ Text.lines result
        pure $ Text.takeWhileEnd (/= '(') $ Text.takeWhile (/= ')') versionLine

venvsPath :: (Concurrent :> es, Driver :> es) => Eff es (Path Abs)
venvsPath = mkPath <$> pipx drvRunOutput ["environment", "--value", "PIPX_LOCAL_VENVS"]

pipxInstall :: App es => PipxPackage -> Eff es ()
pipxInstall p = do
  prerequisites p
  pipx drvRun $ (if isRepo p then ["install", "--force"] else ["upgrade", "--install"]) <> [p.package]
  metadata_ <- getInstalled p
  local <- drvLocal
  for_ metadata_ $ \metadata -> do
    -- track virtual environment
    venvs <- venvsPath
    let envPath = venvs </> metadata.canonicalName
    trkAdd p envPath
    -- Track binaries
    for_ metadata.binaries $ \binary ->
      let binPath = local </> "bin" </> binary.basename
       in trkAdd p binPath

instance Package PipxPackage where
  localVersion = localVersionPipx
  remoteVersion = remoteVersionPipx
  install = installWithPost $ manualVersionInstall pipxInstall
