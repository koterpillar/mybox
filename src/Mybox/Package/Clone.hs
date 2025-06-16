module Mybox.Package.Clone
  ( ClonePackage(..)
  ) where

import qualified Data.Text           as Text

import           Mybox.Driver
import           Mybox.Package.Class
import           Mybox.Prelude
import           Mybox.Utils

data ClonePackage = ClonePackage
  { cpRepo        :: Text
  , cpBranch      :: Maybe Text
  , cpDestination :: Text
  } deriving (Show, Eq)

cpName :: ClonePackage -> Text
cpName ClonePackage {..} = Text.intercalate "#" $ cpRepo : toList cpBranch

cpDestinationPath :: MonadDriver m => ClonePackage -> m Text
cpDestinationPath p = do
  home <- drvHome
  pure $ home <> "/" <> cpDestination p

cpRemote :: ClonePackage -> Text
cpRemote p
  | Text.isPrefixOf "https://" r = r
  | Text.isInfixOf "@" r = r
  | otherwise = "https://github.com/" <> r <> ".git"
  where
    r = cpRepo p

cpGitArgs :: MonadDriver m => [Text] -> ClonePackage -> m (NonEmpty Text)
cpGitArgs args p = do
  dest <- cpDestinationPath p
  pure $ "git" :| (["-C", dest] <> args)

cpRunGit :: MonadDriver m => [Text] -> ClonePackage -> m ()
cpRunGit args p = cpGitArgs args p >>= drvRun

cpRevParse ::
     MonadDriver m
  => Text -- ^ branch name
  -> Bool -- ^ abbrev-ref
  -> ClonePackage
  -> m Text
cpRevParse branch abbrevRef p =
  cpGitArgs (join [["rev-parse"], ["--abbrev-ref" | abbrevRef], [branch]]) p
    >>= drvRunOutput

cpDirectoryExists :: MonadDriver m => ClonePackage -> m Bool
cpDirectoryExists p = cpDestinationPath p >>= drvIsDir

cpLocalVersion :: MonadDriver m => ClonePackage -> m (Maybe Text)
cpLocalVersion p = do
  exists <- cpDirectoryExists p
  if exists
    then Just <$> cpRevParse "HEAD" False p
    else pure Nothing

cpRemoteVersion :: MonadDriver m => ClonePackage -> m Text
cpRemoteVersion p = repoBranchVersion (cpRemote p) (cpBranch p)

cpDefaultRemote :: Text
cpDefaultRemote = "origin"

cpInstall :: MonadDriver m => ClonePackage -> m ()
cpInstall p = do
  destination <- cpDestinationPath p
  exists <- drvIsDir $ destination <> "/.git"
  if exists
    then cpRunGit ["remote", "set-url", cpDefaultRemote, cpRemote p] p
    else do
      drvMkdir $ drvDirname destination
      drvRm destination
      drvRun $ "git" :| ["clone", cpRemote p, destination]
  remoteBranch <-
    case cpBranch p of
      Just branch -> pure $ cpDefaultRemote <> "/" <> branch
      Nothing     -> cpRevParse (cpDefaultRemote <> "/HEAD") True p
  let branch = Text.takeWhileEnd (/= '/') remoteBranch
  cpRunGit ["fetch", "--no-tags", cpDefaultRemote, branch] p
  currentBranch <- cpRevParse "HEAD" True p
  when (currentBranch /= branch) $ cpRunGit ["switch", branch] p
  cpRunGit ["reset", "--hard", remoteBranch] p

instance Package ClonePackage where
  pkgName = cpName
  pkgLocalVersion = cpLocalVersion
  pkgRemoteVersion = cpRemoteVersion
  pkgInstall = cpInstall
