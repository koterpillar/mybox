module Mybox.Driver.Ops where

import           Control.Exception.Safe (MonadMask, bracket)

import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.List.NonEmpty     as NonEmpty
import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Mybox.Driver.Class

import           System.Exit            (ExitCode (..))

-- | Check if a path is executable.
drvIsExecutable :: MonadDriver m => Text -> m Bool
drvIsExecutable path = do
  code <- drvRunOk $ "test" :| ["-x", path]
  pure (code == ExitSuccess)

-- | Check if a path is a regular file.
drvIsFile :: MonadDriver m => Text -> m Bool
drvIsFile path = do
  code <- drvRunOk $ "test" :| ["-f", path]
  pure (code == ExitSuccess)

-- | Check if an executable exists in PATH.
drvExecutableExists :: MonadDriver m => Text -> m Bool
drvExecutableExists exe = do
  code <- drvRunOk $ drvShell $ "command" :| ["-v", exe]
  pure (code == ExitSuccess)

-- | Check if a path is a directory.
drvIsDir :: MonadDriver m => Text -> m Bool
drvIsDir path = do
  code <- drvRunOk $ "test" :| ["-d", path]
  pure (code == ExitSuccess)

-- | Get the current username (FIXME: check root)
drvUsername :: MonadDriver m => m Text
drvUsername = drvRunOutput $ "whoami" :| []

-- | Get the home directory for the user (FIXME: check root)
drvHome :: MonadDriver m => m Text
drvHome = drvRunOutput $ drvShell $ "eval" :| ["echo", "~"]

-- | Get the local directory for the user (FIXME: check root)
drvLocal :: MonadDriver m => m Text
drvLocal = do
  home <- drvHome
  pure (home <> "/.local")

-- | Remove a file or directory.
drvRm :: MonadDriver m => Text -> m ()
drvRm path = drvRun $ "rm" :| ["-r", "-f", path]

-- | Create directories recursively.
drvMkdir :: MonadDriver m => Text -> m ()
drvMkdir path = drvRun $ "mkdir" :| ["-p", path]

-- | Create a symbolic link from source to target.
drvLink :: MonadDriver m => Text -> Text -> m ()
drvLink source target = do
  drvMkdir $ drvDirname target
  drvRm target
  drvRun $ "ln" :| ["-s", "-f", source, target]

-- | Copy a file or directory recursively
drvCopy :: MonadDriver m => Text -> Text -> m ()
drvCopy source target = do
  drvMkdir $ drvDirname target
  drvRm target
  drvRun $ "cp" :| ["-R", "-f", source, target]

drvTemp_ :: (MonadDriver m, MonadMask m) => Bool -> (Text -> m a) -> m a
drvTemp_ isDirectory =
  bracket (drvRunOutput $ "mktemp" :| ["-d" | isDirectory]) drvRm

-- | Execute an action with a temporary file, cleaning up afterwards
drvTempFile :: (MonadDriver m, MonadMask m) => (Text -> m a) -> m a
drvTempFile = drvTemp_ False

-- | Execute an action with a temporary directory, cleaning up afterwards
drvTempDir :: (MonadDriver m, MonadMask m) => (Text -> m a) -> m a
drvTempDir = drvTemp_ True

-- | Helper: dirname (get parent directory as text)
drvDirname :: Text -> Text
drvDirname path =
  case Text.splitOn "/" path of
    []  -> "."
    [_] -> "."
    xs  -> Text.intercalate "/" (init xs)

drvShell :: Args -> Args
drvShell args =
  "sh" :| ["-c", Text.intercalate " " $ NonEmpty.toList args] -- FIXME quote args
