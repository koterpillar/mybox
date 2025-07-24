module Mybox.Driver.Ops where

import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful.Exception

import Mybox.Driver.Class
import Mybox.Prelude

data TestOp = IsExecutable | IsDirectory | IsSymlink | IsFile

drvTest :: Driver :> es => TestOp -> Text -> Eff es Bool
drvTest op path = do
  let opStr = case op of
        IsExecutable -> "-x"
        IsDirectory -> "-d"
        IsSymlink -> "-L"
        IsFile -> "-f"
  code <- drvRunOk $ "test" :| [opStr, path]
  pure (code == ExitSuccess)

drvIsExecutable :: Driver :> es => Text -> Eff es Bool
drvIsExecutable = drvTest IsExecutable

drvIsFile :: Driver :> es => Text -> Eff es Bool
drvIsFile = drvTest IsFile

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

-- | Check if a path is a directory.
drvIsDir :: Driver :> es => Text -> Eff es Bool
drvIsDir = drvTest IsDirectory

-- | Check if a path is a symbolic link.
drvIsSymlink :: Driver :> es => Text -> Eff es Bool
drvIsSymlink = drvTest IsSymlink

-- | Get the current username (FIXME: check root)
drvUsername :: Driver :> es => Eff es Text
drvUsername = drvRunOutput $ "whoami" :| []

-- | Get the home directory for the user (FIXME: check root)
drvHome :: Driver :> es => Eff es Text
drvHome = drvRunOutput $ shell $ "eval" :| ["echo", "~"]

-- | Get the local directory for the user (FIXME: check root)
drvLocal :: Driver :> es => Eff es Text
drvLocal = do
  home <- drvHome
  pure (home </> ".local")

-- | Remove a file or directory.
drvRm :: Driver :> es => Text -> Eff es ()
drvRm path = drvRun $ "rm" :| ["-r", "-f", path]

-- | Create directories recursively.
drvMkdir :: Driver :> es => Text -> Eff es ()
drvMkdir path = unlessM (drvIsSymlink path) $ drvRun $ "mkdir" :| ["-p", path]

-- | Create a symbolic link from source to target.
drvLink :: Driver :> es => Text -> Text -> Eff es ()
drvLink source target = do
  drvMkdir $ pDirname target
  drvRm target
  drvRun $ "ln" :| ["-s", "-f", source, target]

-- | Copy a file or directory recursively
drvCopy :: Driver :> es => Text -> Text -> Eff es ()
drvCopy source target = do
  drvMkdir $ pDirname target
  drvRm target
  drvRun $ "cp" :| ["-R", "-f", source, target]

drvTemp_ :: Driver :> es => Bool -> Eff es Text
drvTemp_ isDirectory = drvRunOutput $ "mktemp" :| ["-d" | isDirectory]

drvTempFile :: Driver :> es => (Text -> Eff es a) -> Eff es a
drvTempFile = bracket (drvTemp_ False) drvRm

drvTempDir :: Driver :> es => (Text -> Eff es a) -> Eff es a
drvTempDir = bracket (drvTemp_ True) drvRm

drvReadFile :: Driver :> es => Text -> Eff es Text
drvReadFile path = drvRunOutput $ "cat" :| [path]

drvWriteFile :: Driver :> es => Text -> Text -> Eff es ()
drvWriteFile path content = do
  drvMkdir $ pDirname path
  drvRm path
  drvRun $ shellRaw $ "echo " <> shellQuote content <> " > " <> shellQuote path

drvWriteBinaryFile :: Driver :> es => Text -> LBS.ByteString -> Eff es ()
drvWriteBinaryFile path content = do
  drvMkdir $ pDirname path
  drvRm path
  let base64 = Text.decodeUtf8 $ Base64.encode $ LBS.toStrict content
  drvRun $ shellRaw $ "echo " <> shellQuote base64 <> " | base64 -d > " <> shellQuote path

drvMakeExecutable :: Driver :> es => Text -> Eff es ()
drvMakeExecutable path = drvRun $ "chmod" :| ["+x", path]

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

drvFind :: Driver :> es => Text -> FindOptions -> Eff es (Set Text)
drvFind path fo = do
  let maybeArg :: Text -> Maybe [Text] -> [Text]
      maybeArg _ Nothing = []
      maybeArg arg (Just [v]) = [arg, v]
      maybeArg arg (Just vs) = ["("] <> intercalate ["-o"] [[arg, v] | v <- vs] <> [")"]
      isNul = (== '\0')
      args =
        [path, "-mindepth", "1"]
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
      pure $ Set.fromList names

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

drvRedirectLocation :: Driver :> es => Text -> Eff es Text
drvRedirectLocation = drvUrlProperty "%{url_effective}"

drvUrlProperty :: Driver :> es => Text -> Text -> Eff es Text
drvUrlProperty property url = do
  drvRunOutput $
    "curl"
      :| [ "--fail"
         , "--silent"
         , "--show-error"
         , "--location"
         , "--output"
         , "/dev/null"
         , "--write-out"
         , property
         , url
         ]

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

shellRaw :: Text -> Args
shellRaw args = "sh" :| ["-c", args]

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
  safe c = isAlphaNum c || c == '_'

sudo :: Args -> Args
sudo args = "sudo" :| toList args

env :: [(Text, Text)] -> Args -> Args
env vars args = "env" :| map mkVar vars ++ toList args
 where
  mkVar (k, v) = k <> "=" <> v
