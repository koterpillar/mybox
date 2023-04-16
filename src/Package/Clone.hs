module Package.Clone
  ( Clone(..)
  ) where

import           Data.Text            (Text)
import qualified Data.Text            as Text

import           System.Process.Typed

import           Driver
import           Driver.Actions

import           Package

data Clone =
  Clone
    { cloneRepo        :: Text
    , cloneDestination :: FilePath
    }
  deriving (Eq, Show)

cloneRemote :: Clone -> Text
cloneRemote Clone {cloneRepo = repo}
  | Text.isPrefixOf "https://" repo = repo
  | Text.isInfixOf "@" repo = repo
  | otherwise = "https://github.com/" <> repo <> ".git"

instance Package Clone where
  pkRemoteVersion _ pk = do
    remote <-
      fmap procDecode $
      readProcessStdout_ $
      procText $ git :| ["ls-remote", cloneRemote pk, "HEAD"]
    pure $ Text.takeWhile (/= '\t') remote
  pkLocalVersion drv pk = do
    exists <- directoryExists drv pk
    if not exists
      then pure Nothing
      else Just <$> drvRunOutput (gitArgs pk ["rev-parse", "HEAD"]) drv
  pkInstall drv pk = do
    let remote = cloneRemote pk
    exists <- directoryExists drv pk
    if exists
      then do
        drvRun (gitArgs pk ["remote", "set-url", "origin", remote]) drv
        drvRun (gitArgs pk ["fetch"]) drv
        defaultBranch <-
          Text.takeWhileEnd (/= '/') <$>
          drvRunOutput
            (gitArgs pk ["rev-parse", "--abbrev-ref", "origin/HEAD"])
            drv
        drvRun (gitArgs pk ["switch", defaultBranch]) drv
        drvRun (gitArgs pk ["reset", "--hard", "origin/" <> defaultBranch]) drv
      else do
        -- FIXME: create parent directories
        drvRun (git :| ["clone", remote, Text.pack $ cloneDestination pk]) drv

git :: Text
git = "git"

-- FIXME: destination should be converted to absolute using drvHome
gitArgs :: Clone -> [Text] -> NonEmpty Text
gitArgs Clone {cloneDestination = dest} args =
  git :| ["-C", Text.pack dest] ++ args

directoryExists :: Driver -> Clone -> IO Bool
directoryExists drv pk = drvIsDirectory (cloneDestination pk) drv
