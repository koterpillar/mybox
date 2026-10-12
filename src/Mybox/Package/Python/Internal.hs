module Mybox.Package.Python.Internal where

import Data.Char (isAlphaNum)
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Archive
import Mybox.Package.Class
import Mybox.Package.Forge hiding (repo)
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Tracker

data PythonPackage = PythonPackage
  { package :: Text
  , post :: [Text]
  }
  deriving (Eq, Generic, Show)

mkPythonPackage :: Text -> PythonPackage
mkPythonPackage package = PythonPackage{package, post = []}

instance PackageName PythonPackage where
  splitName = genericSplitName' @'[] @'["package"]

instance FromJSON PythonPackage where
  parseJSON = withObjectTotal "PythonPackage" $ do
    package <- takeField "python"
    post <- takePost
    pure PythonPackage{..}

instance ToJSON PythonPackage where
  toJSON p = object $ ["python" .= p.package] <> postToJSON p

repo :: PythonPackage -> Maybe (Text, Maybe Text)
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

isRepo :: PythonPackage -> Bool
isRepo = isJust . repo

newtype PythonList = PythonList {packages :: [PythonInstalledPackage]} deriving (Show)

instance FromJSON PythonList where
  parseJSON = withObject "PythonList" $ \obj -> do
    packages <- obj .: "venvs"
    pure $ PythonList{packages = toList (packages :: Map Text PythonInstalledPackage)}

prerequisites :: App es => PythonPackage -> Eff es ()
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
    (mkForgePackage "pypa/pipx")
      { archive = emptyArchiveFields{binaries = ["pipx"], raw = Left "pipx"}
      }
  when (isRepo p) ensureGit

pipx :: (Concurrent :> es, Driver :> es) => (Args -> Eff es a) -> [Text] -> Eff es a
pipx run args = drvAtomic "pipx" $ run $ "pipx" :| args

data PythonInstalledPackage = PythonInstalledPackage
  { name :: Text
  , canonicalName :: Text
  , version :: Maybe Text
  , binaries :: [Path Abs]
  }
  deriving (Show)

instance FromJSON PythonInstalledPackage where
  parseJSON = withObject "PythonInstalledPackage" $ \obj -> do
    metadata <- obj .: "metadata"
    mainPackage <- metadata .: "main_package"
    name <- mainPackage .: "package_or_url"
    canonicalName <- mainPackage .: "package"
    version <- mainPackage .:? "package_version"
    binaries <- mainPackage .: "app_paths" >>= traverse (.: "__Path__")
    pure $ PythonInstalledPackage{..}

getInstalled :: (Concurrent :> es, Driver :> es) => PythonPackage -> Eff es (Maybe PythonInstalledPackage)
getInstalled p = do
  pipxListOutput <- pipx drvRunOutput ["list", "--json"]
  allMetadata <- jsonDecode @PythonList "pipx output" pipxListOutput
  pure $ find (\pkg -> pkg.name == p.package) allMetadata.packages

localVersionPython :: (Concurrent :> es, Driver :> es, Tracker :> es) => PythonPackage -> Eff es (Maybe Text)
localVersionPython p = do
  if isRepo p
    then manualVersion p -- pipx doesn't store Git commit, just the version field from the package
    else do
      metadata <- getInstalled p
      pure $ metadata >>= (.version)

remoteVersionPython :: App es => PythonPackage -> Eff es Text
remoteVersionPython p = do
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

pythonInstall :: App es => PythonPackage -> Eff es ()
pythonInstall p = do
  prerequisites p
  pipx drvRun $ (if isRepo p then ["install", "--force"] else ["upgrade", "--install"]) <> [p.package]
  metadata_ <- getInstalled p
  local <- drvLocal False
  for_ metadata_ $ \metadata -> do
    -- track virtual environment
    venvs <- venvsPath
    let envPath = venvs </> metadata.canonicalName
    trkAdd p envPath
    -- Track binaries
    for_ metadata.binaries $ \binary ->
      let binPath = local </> "bin" </> binary.basename
       in trkAdd p binPath

instance Package PythonPackage where
  localVersion = localVersionPython
  remoteVersion = remoteVersionPython
  install = installWithPost $ manualVersionInstall pythonInstall
