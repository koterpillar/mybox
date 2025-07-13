module Mybox.Driver.Ops where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Driver.Class
import Mybox.Prelude

-- | Check if a path is executable.
drvIsExecutable :: Driver :> es => Text -> Eff es Bool
drvIsExecutable path = do
  code <- drvRunOk $ "test" :| ["-x", path]
  pure (code == ExitSuccess)

-- | Check if a path is a regular file.
drvIsFile :: Driver :> es => Text -> Eff es Bool
drvIsFile path = do
  code <- drvRunOk $ "test" :| ["-f", path]
  pure (code == ExitSuccess)

-- | Check if an executable exists in PATH.
drvExecutableExists :: Driver :> es => Text -> Eff es Bool
drvExecutableExists exe = do
  code <- drvRunOk $ shell $ "command" :| ["-v", exe]
  pure (code == ExitSuccess)

-- | Check if a path is a directory.
drvIsDir :: Driver :> es => Text -> Eff es Bool
drvIsDir path = do
  code <- drvRunOk $ "test" :| ["-d", path]
  pure (code == ExitSuccess)

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
drvMkdir path = drvRun $ "mkdir" :| ["-p", path]

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

drvTempFile :: Driver :> es => Eff es Text
drvTempFile = drvTemp_ False

drvTempDir :: Driver :> es => Eff es Text
drvTempDir = drvTemp_ True

drvReadFile :: Driver :> es => Text -> Eff es Text
drvReadFile path = drvRunOutput $ "cat" :| [path]

drvWriteFile :: Driver :> es => Text -> Text -> Eff es ()
drvWriteFile path content = do
  drvMkdir $ pDirname path
  drvRm path
  drvRun $ shellRaw $ "echo " <> shellQuote content <> " > " <> shellQuote path

drvMakeExecutable :: Driver :> es => Text -> Eff es ()
drvMakeExecutable path = drvRun $ "chmod" :| ["+x", path]

data FindOptions = FindOptions
  { maxDepth :: Maybe Int
  , onlyFiles :: Bool
  , name :: Maybe [Text]
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
      maybeArg arg (Just vs) = [arg, Text.intercalate "," vs]
  let args =
        [path, "-mindepth", "1"]
          ++ maybeArg "-maxdepth" (pure . Text.pack . show <$> fo.maxDepth)
          ++ maybeArg "-name" fo.name
          ++ maybeArg
            "-type"
            ( if fo.onlyFiles
                then Just ["f", "l"]
                else Nothing
            )
          ++ ["-print0"]
  o <- drvRunOutput $ "find" :| args
  pure $ Set.fromList $ Text.split (== '\0') o

drvUrlEtag :: Driver :> es => Text -> Eff es Text
drvUrlEtag url =
  drvRunOutput $
    "curl"
      :| [ "--fail"
         , "--silent"
         , "--show-error"
         , "--location"
         , "--output"
         , "/dev/null"
         , "--write-out"
         , "%header{etag}"
         , url
         ]

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
