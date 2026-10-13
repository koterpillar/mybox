module Mybox.Package.Python.Internal where

import Data.Char (isAlphaNum)
import Data.Text qualified as Text
import Toml qualified

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

data PythonInstalledPackage = PythonInstalledPackage
  { name :: Text
  , version :: Text
  }
  deriving (Eq, Show)

-- | Parse @uv tool list@ output: @name vVERSION@ lines, each followed by
-- @- binary@ lines.
parseToolList :: Text -> [PythonInstalledPackage]
parseToolList = mapMaybe parse . Text.lines
 where
  parse l = do
    guard $ not $ "-" `Text.isPrefixOf` l
    let (name, version) = Text.breakOn " v" l
    guard $ not $ Text.null version
    pure PythonInstalledPackage{name, version = Text.drop 2 version}

prerequisites :: App es => PythonPackage -> Eff es ()
prerequisites p = do
  queueInstall $
    (mkForgePackage "astral-sh/uv")
      { archive = emptyArchiveFields{binaries = ["uv"]}
      }
  when (isRepo p) ensureGit

uv :: (Args -> Eff es a) -> [Text] -> Eff es a
uv run args = run $ "uv" :| args

toolsDir :: Driver :> es => Eff es (Path Abs)
toolsDir = mkPath <$> uv drvRunOutput ["tool", "dir"]

-- | Contents of @uv-receipt.toml@ that we use.
data Receipt = Receipt
  { requirements :: [Requirement]
  , entrypoints :: [Entrypoint]
  }
  deriving (Show)

data Requirement = Requirement
  { git :: Maybe Text
  }
  deriving (Show)

newtype Entrypoint = Entrypoint {installPath :: Text} deriving (Show)

receiptCodec :: Toml.TomlCodec Receipt
receiptCodec = Toml.table toolCodec "tool"
 where
  toolCodec =
    Receipt
      <$> Toml.list requirementCodec "requirements" Toml..= (.requirements)
      <*> Toml.list entrypointCodec "entrypoints" Toml..= (.entrypoints)
  requirementCodec = Requirement <$> Toml.dioptional (Toml.text "git") Toml..= (.git)
  entrypointCodec = Entrypoint <$> Toml.text "install-path" Toml..= (.installPath)

readReceipt :: Driver :> es => Path Abs -> PythonInstalledPackage -> Eff es Receipt
readReceipt tools t = do
  let path = tools </> t.name </> "uv-receipt.toml"
  contents <- drvReadFile path
  either (terror . Toml.prettyTomlDecodeErrors) pure $ Toml.decode receiptCodec contents

getInstalled :: Driver :> es => PythonPackage -> Eff es (Maybe PythonInstalledPackage)
getInstalled p = do
  tools <- toolsDir
  installed <- parseToolList <$> uv drvRunOutput ["tool", "list"]
  findM (matches tools) installed
 where
  -- Git packages are identified by the URL in the receipt (uv appends @?rev=@ for a ref), as the tool is named after the package
  matches tools t = case repo p of
    Nothing -> pure $ t.name == p.package
    Just (url, _) -> do
      receipt <- readReceipt tools t
      pure $ any ((== Just url) . fmap (Text.takeWhile (/= '?')) . (.git)) receipt.requirements

localVersionPython :: (Driver :> es, Tracker :> es) => PythonPackage -> Eff es (Maybe Text)
localVersionPython p = do
  if isRepo p
    then manualVersion p -- uv doesn't store Git commit, just the version field from the package
    else fmap (.version) <$> getInstalled p

remoteVersionPython :: App es => PythonPackage -> Eff es Text
remoteVersionPython p = do
  prerequisites p
  case repo p of
    Just (r, b) -> drvRepoBranchVersion r b
    Nothing -> do
      result <-
        uv drvRunOutput ["run", "--no-project", "--with", "pip", "pip", "index", "versions", p.package]
      maybe (terror "Cannot parse pip output") pure $ do
        versionLine <- listToMaybe $ Text.lines result
        pure $ Text.takeWhileEnd (/= '(') $ Text.takeWhile (/= ')') versionLine

pythonInstall :: App es => PythonPackage -> Eff es ()
pythonInstall p = do
  prerequisites p
  uv drvRun ["tool", "install", "--force", "--upgrade", p.package]
  metadata_ <- getInstalled p
  tools <- toolsDir
  for_ metadata_ $ \metadata -> do
    -- track virtual environment
    trkAdd p $ tools </> metadata.name
    -- Track binaries
    receipt <- readReceipt tools metadata
    for_ receipt.entrypoints $ \entrypoint ->
      trkAdd p $ mkPath entrypoint.installPath

instance Package PythonPackage where
  localVersion = localVersionPython
  remoteVersion = remoteVersionPython
  install = installWithPost $ manualVersionInstall pythonInstall
