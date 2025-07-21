module Mybox.Package.Pipx (PipxPackage (..), mkPipxPackage) where

import Data.Map (Map)
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker
import Mybox.Utils

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
  parseJSON = withObject "PipxPackage" $ \o -> do
    package <- o .: "pipx"
    post <- parsePost o
    pure PipxPackage{..}

instance ToJSON PipxPackage where
  toJSON p = object $ ["pipx" .= p.package] <> postToJSON p

repo :: PipxPackage -> Maybe Text
repo p = Text.stripPrefix "git+" p.package

isRepo :: PipxPackage -> Bool
isRepo = isJust . repo

newtype PipxList = PipxList {packages :: [PipxInstalledPackage]} deriving (Show)

instance FromJSON PipxList where
  parseJSON = withObject "PipxList" $ \obj -> do
    packages <- obj .: "venvs"
    pure $ PipxList{packages = toList (packages :: Map Text PipxInstalledPackage)}

prerequisites :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => Eff es ()
prerequisites = do
  -- FIXME: Install pipx
  os <- drvOS
  let packages = case os of
        Linux _ -> ["python3-pip"]
        MacOS -> []
  for_ packages $ \package ->
    queueInstall $ mkSystemPackage package

data PipxInstalledPackage = PipxInstalledPackage {name :: Text, version :: Maybe Text, binaries :: [Text]}
  deriving (Show)

instance FromJSON PipxInstalledPackage where
  parseJSON = withObject "PipxInstalledPackage" $ \obj -> do
    metadata <- obj .: "metadata"
    mainPackage <- metadata .: "main_package"
    name <- mainPackage .: "package_or_url"
    version <- mainPackage .:? "package_version"
    binaries <- mainPackage .: "app_paths" >>= traverse (.: "__Path__")
    pure $ PipxInstalledPackage{name = name, version = version, binaries = binaries}

getInstalled :: Driver :> es => PipxPackage -> Eff es (Maybe PipxInstalledPackage)
getInstalled p = do
  pipxListOutput <- drvRunOutput $ "pipx" :| ["list", "--json"]
  allMetadata <- jsonDecode @PipxList "pipx output" pipxListOutput
  pure $ find (\pkg -> pkg.name == p.package) allMetadata.packages

localVersionPipx :: Driver :> es => PipxPackage -> Eff es (Maybe Text)
localVersionPipx p = do
  if isRepo p
    then manualVersion p -- pipx doesn't store Git commit, just the version field from the package
    else do
      metadata <- getInstalled p
      pure $ metadata >>= (.version)

remoteVersionPipx :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => PipxPackage -> Eff es Text
remoteVersionPipx p =
  case repo p of
    Just r -> repoBranchVersion r Nothing
    Nothing -> do
      prerequisites
      result <-
        drvRunOutput $
          "python3" :| ["-m", "pip", "index", "versions", p.package]
      maybe (terror "Cannot parse pip output") pure $ do
        versionLine <- listToMaybe $ Text.lines result
        pure $ Text.takeWhileEnd (/= '(') $ Text.takeWhile (/= ')') versionLine

pipxInstall :: (Driver :> es, TrackerSession :> es) => PipxPackage -> Eff es ()
pipxInstall p = do
  drvRun $ "pipx" :| ((if isRepo p then ["install", "--force"] else ["upgrade", "--install"]) <> [p.package])
  -- track virtual environment
  local <- drvLocal
  let envPath = local </> "pipx" </> "venvs" </> p.package
  trkAdd p envPath
  -- Track binaries
  metadata_ <- getInstalled p
  for_ metadata_ $ \metadata ->
    for_ metadata.binaries $ \binary ->
      for_ (pFilename binary) $ \binName ->
        let binPath = local </> "bin" </> binName
         in trkAdd p binPath

instance Package PipxPackage where
  localVersion = localVersionPipx
  remoteVersion = remoteVersionPipx
  install = installWithPost $ manualVersionInstall pipxInstall
