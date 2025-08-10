module Mybox.Driver.Ops where

import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Mybox.Driver.Class
import Mybox.Platform
import Mybox.Prelude

data TestOp = IsExecutable | IsDirectory | IsSymlink | IsFile

drvTest :: (Anchor a, Driver :> es) => TestOp -> Path a -> Eff es Bool
drvTest op path = do
  let opStr = case op of
        IsExecutable -> "-x"
        IsDirectory -> "-d"
        IsSymlink -> "-L"
        IsFile -> "-f"
  code <- drvRunOk $ "test" :| [opStr, path.text]
  pure (code == ExitSuccess)

drvIsExecutable :: (Anchor a, Driver :> es) => Path a -> Eff es Bool
drvIsExecutable = drvTest IsExecutable

drvIsFile :: (Anchor a, Driver :> es) => Path a -> Eff es Bool
drvIsFile = drvTest IsFile

-- | Check if a path is a directory.
drvIsDir :: (Anchor a, Driver :> es) => Path a -> Eff es Bool
drvIsDir = drvTest IsDirectory

-- | Check if a path is a symbolic link.
drvIsSymlink :: (Anchor a, Driver :> es) => Path a -> Eff es Bool
drvIsSymlink = drvTest IsSymlink

-- | Check if an executable exists in PATH.
drvExecutableExists :: Driver :> es => Text -> Eff es Bool
drvExecutableExists exe = do
  result <- drvRunOutputExit $ shell $ "command" :| ["-v", exe]
  pure (result.exit == ExitSuccess)

drvFindExecutable :: Driver :> es => [Text] -> Eff es Text
drvFindExecutable candidates = go candidates
 where
  go [] = terror $ "Neither of " <> Text.intercalate ", " candidates <> " found in PATH."
  go (exe : executables) = do
    exists <- drvExecutableExists exe
    if exists
      then pure exe
      else drvFindExecutable executables

-- | Get the current username.
drvUsername :: Driver :> es => Eff es Text
drvUsername = drvRunOutput $ "whoami" :| []

-- | Get the current working directory.
drvCurrentDir :: Driver :> es => Eff es (Path Abs)
drvCurrentDir = mkPath <$> drvRunOutput ("pwd" :| [])

-- | Get the home directory for the user.
drvHome :: Driver :> es => Eff es (Path Abs)
drvHome = fmap mkPath $ drvRunOutput $ shell $ "eval" :| ["echo", "~"]

-- | Get the local directory for the user.
drvLocal :: Driver :> es => Eff es (Path Abs)
drvLocal = fmap (<//> pLocal) drvHome

drvMyboxState :: Driver :> es => Eff es (Path Abs)
drvMyboxState = fmap (<//> pMyboxState) drvHome

-- | Remove a file or directory.
drvRm :: (Anchor a, Driver :> es) => Path a -> Eff es ()
drvRm path = drvRun $ "rm" :| ["-r", "-f", path.text]

-- | Create directories recursively.
drvMkdir :: (Anchor a, Driver :> es) => Path a -> Eff es ()
drvMkdir path = unlessM (drvIsSymlink path) $ drvRun $ "mkdir" :| ["-p", path.text]

-- | Create a symbolic link from source to target.
drvLink :: (Anchor a1, Anchor a2, Driver :> es) => Path a1 -> Path a2 -> Eff es ()
drvLink source target = do
  drvMkdir target.dirname
  drvRm target
  drvRun $ "ln" :| ["-s", "-f", source.text, target.text]

-- | Copy a file or directory recursively
drvCopy :: (Anchor a1, Anchor a2, Driver :> es) => Path a1 -> Path a2 -> Eff es ()
drvCopy source target = do
  drvMkdir target.dirname
  drvRm target
  drvRun $ "cp" :| ["-R", "-f", source.text, target.text]

drvTemp_ :: Driver :> es => Bool -> Eff es (Path Abs)
drvTemp_ isDirectory = fmap mkPath $ drvRunOutput $ "mktemp" :| ["-d" | isDirectory]

drvTempFile :: Driver :> es => (Path Abs -> Eff es a) -> Eff es a
drvTempFile = bracket (drvTemp_ False) drvRm

drvTempDir :: Driver :> es => (Path Abs -> Eff es a) -> Eff es a
drvTempDir = bracket (drvTemp_ True) drvRm

drvReadFile :: (Anchor a, Driver :> es) => Path a -> Eff es Text
drvReadFile path = drvRunOutput $ "cat" :| [path.text]

drvWriteFile :: (Anchor a, Driver :> es) => Path a -> Text -> Eff es ()
drvWriteFile path content = do
  drvMkdir path.dirname
  drvRm path
  drvRun $ shellRaw $ "echo " <> shellQuote content <> " > " <> shellQuote path.text

drvWriteBinaryFile :: (Anchor a, Driver :> es) => Path a -> LBS.ByteString -> Eff es ()
drvWriteBinaryFile path content = do
  drvMkdir path.dirname
  drvRm path
  let base64 = Text.decodeUtf8 $ Base64.encode $ LBS.toStrict content
  drvRun $ shellRaw $ "echo " <> shellQuote base64 <> " | base64 -d > " <> shellQuote path.text

drvMakeExecutable :: (Anchor a, Driver :> es) => Path a -> Eff es ()
drvMakeExecutable path = drvRun $ "chmod" :| ["+x", path.text]

data FindOptions = FindOptions
  { maxDepth :: Maybe Int
  , onlyFiles :: Bool
  , names :: Maybe [Text]
  }
  deriving (Eq, Ord, Show)

instance Semigroup FindOptions where
  FindOptions d1 f1 n1 <> FindOptions d2 f2 n2 =
    FindOptions (d2 <|> d1) (f1 || f2) (n1 <> n2)

instance Monoid FindOptions where
  mempty = FindOptions Nothing False Nothing

drvFind :: (Anchor a, Driver :> es) => Path a -> FindOptions -> Eff es (Set (Path a))
drvFind path fo = do
  let maybeArg :: Text -> Maybe [Text] -> [Text]
      maybeArg _ Nothing = []
      maybeArg arg (Just [v]) = [arg, v]
      maybeArg arg (Just vs) = ["("] <> intercalate ["-o"] [[arg, v] | v <- vs] <> [")"]
      isNul = (== '\0')
      args =
        [path.text, "-mindepth", "1"]
          ++ maybeArg "-maxdepth" (pure . Text.pack . show <$> fo.maxDepth)
          ++ maybeArg "-name" fo.names
          ++ maybeArg
            "-type"
            ( if fo.onlyFiles
                then Just ["f", "l"]
                else Nothing
            )
          ++ ["-print0"]
  o <- drvRunOutput $ "find" :| args
  if Text.null o
    then pure Set.empty
    else do
      let names = Text.split isNul $ Text.dropWhileEnd isNul o
      pure $ Set.fromList $ map mkPath names

drvEnv :: Driver :> es => Text -> Eff es (Maybe Text)
drvEnv name = do
  result <- drvRunOutputExit $ shellRaw $ "echo $" <> name
  pure $
    if result.exit == ExitSuccess && not (Text.null result.output)
      then Just result.output
      else Nothing

drvGithubToken :: Driver :> es => Eff es Text
drvGithubToken = drvEnv "GITHUB_TOKEN" `fromMaybeOrMM` drvRunOutput ("gh" :| ["auth", "token"])

drvUrlEtag :: Driver :> es => Text -> Eff es Text
drvUrlEtag = drvUrlProperty "%header{etag}"

drvHttpGet :: Driver :> es => Text -> Eff es Text
drvHttpGet url = drvRunOutput $ curl [] url

drvRedirectLocation :: Driver :> es => Text -> Eff es Text
drvRedirectLocation = drvUrlProperty "%{url_effective}"

drvUrlProperty :: Driver :> es => Text -> Text -> Eff es Text
drvUrlProperty property = drvRunOutput . curl ["--head", "-o", "/dev/null", "--write-out", property]

drvRepoBranchVersion ::
  Driver :> es =>
  -- | Repository
  Text ->
  -- | Branch
  Maybe Text ->
  Eff es Text
drvRepoBranchVersion repo_ branch_ = do
  let repo = fromMaybe repo_ $ Text.stripPrefix "git+" repo_
  let branch = fromMaybe "HEAD" branch_
  output <- drvRunOutput $ "git" :| ["ls-remote", repo, branch]
  case Text.words output of
    [ref, _] -> pure ref
    _ -> terror $ "Failed to parse git ls-remote output: " <> output

drvArchitecture :: Driver :> es => Eff es Architecture
drvArchitecture = drvRunOutput ("uname" :| ["-m"]) >>= either throwString pure . parseArchitecture

pOSRelease :: Path Abs
pOSRelease = pRoot </> "etc" </> "os-release"

drvOS :: Driver :> es => Eff es OS
drvOS = do
  osStr <- drvRunOutput ("uname" :| [])
  case osStr of
    "Linux" -> do
      distributionStr <- parseOsRelease <$> drvReadFile pOSRelease
      distribution <- case distributionStr of
        "debian" -> pure $ Debian "debian"
        "ubuntu" -> pure $ Debian "ubuntu"
        "fedora" -> pure Fedora
        _ -> terror $ "Unsupported Linux distribution: " <> distributionStr
      pure $ Linux distribution
     where
      parseOsRelease :: Text -> Text
      parseOsRelease contents = fromMaybe (terror "Failed to parse /etc/os-release") $ listToMaybe $ do
        line <- Text.lines contents
        [k, v] <- pure $ Text.splitOn "=" line
        guard (k == "ID")
        if Text.isPrefixOf "\"" v && Text.isSuffixOf "\"" v
          then pure $ Text.drop 1 $ Text.dropEnd 1 v
          else pure v
    "Darwin" -> pure MacOS
    _ -> terror $ "Unsupported OS: " <> osStr

drvHostname :: Driver :> es => Eff es Text
drvHostname = drvRunOutput $ "uname" :| ["-n"]

shellRaw :: Text -> Args
shellRaw args = "/bin/sh" :| ["-c", args]

shell :: Args -> Args
shell = shellRaw . shellJoin

shellJoin :: (Foldable f, Functor f) => f Text -> Text
shellJoin = Text.unwords . toList . fmap shellQuote

shellQuote :: Text -> Text
shellQuote t
  | Text.all safe t && not (Text.null t) = t
  | otherwise = q <> Text.intercalate "'\"'\"'" (Text.splitOn q t) <> q
 where
  q = "'"
  safe c = isAlphaNum c || c == '_' || c == '/'

sudo :: Args -> Args
sudo args = "sudo" :| toList args

curl :: [Text] -> Text -> Args
curl args url = "curl" :| ["-fsSL"] <> args <> [url]

env :: [(Text, Text)] -> Args -> Args
env vars args = "env" :| map mkVar vars ++ toList args
 where
  mkVar (k, v) = k <> "=" <> v
