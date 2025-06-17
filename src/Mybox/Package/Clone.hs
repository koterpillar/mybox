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

cpDestinationPath :: Driver :> es => ClonePackage -> Eff es Text
cpDestinationPath p = do
  home <- drvHome
  pure $ home </> cpDestination p

cpRemote :: ClonePackage -> Text
cpRemote p
  | Text.isPrefixOf "https://" r = r
  | Text.isInfixOf "@" r = r
  | otherwise = "https://github.com/" <> r <> ".git"
  where
    r = cpRepo p

cpGitArgs :: Driver :> es => [Text] -> ClonePackage -> Eff es (NonEmpty Text)
cpGitArgs args p = do
  dest <- cpDestinationPath p
  pure $ "git" :| (["-C", dest] <> args)

cpRunGit :: Driver :> es => [Text] -> ClonePackage -> Eff es ()
cpRunGit args p = cpGitArgs args p >>= drvRun

cpRevParse ::
     Driver :> es
  => Text -- ^ branch name
  -> Bool -- ^ abbrev-ref
  -> ClonePackage
  -> Eff es Text
cpRevParse branch abbrevRef p =
  cpGitArgs (join [["rev-parse"], ["--abbrev-ref" | abbrevRef], [branch]]) p
    >>= drvRunOutput

cpDirectoryExists :: Driver :> es => ClonePackage -> Eff es Bool
cpDirectoryExists p = cpDestinationPath p >>= drvIsDir

cpLocalVersion :: Driver :> es => ClonePackage -> Eff es (Maybe Text)
cpLocalVersion p = do
  exists <- cpDirectoryExists p
  if exists
    then Just <$> cpRevParse "HEAD" False p
    else pure Nothing

cpRemoteVersion :: Driver :> es => ClonePackage -> Eff es Text
cpRemoteVersion p = repoBranchVersion (cpRemote p) (cpBranch p)

cpDefaultRemote :: Text
cpDefaultRemote = "origin"

cpRemoteBranch :: Text -> Text
cpRemoteBranch b = cpDefaultRemote <> "/" <> b

cpInstall :: Driver :> es => ClonePackage -> Eff es ()
cpInstall p = do
  destination <- cpDestinationPath p
  exists <- drvIsDir $ destination </> ".git"
  if exists
    then cpRunGit ["remote", "set-url", cpDefaultRemote, cpRemote p] p
    else do
      drvMkdir $ drvDirname destination
      drvRm destination
      drvRun $ "git" :| ["clone", cpRemote p, destination]
  remoteBranch <-
    case cpBranch p of
      Just branch -> pure $ cpRemoteBranch branch
      Nothing     -> cpRevParse (cpRemoteBranch "HEAD") True p
  let branch = Text.takeWhileEnd (/= '/') remoteBranch
  cpRunGit ["fetch", "--no-tags", cpDefaultRemote, branch] p
  currentBranch <- cpRevParse "HEAD" True p
  when (currentBranch /= branch) $ cpRunGit ["switch", branch] p
  cpRunGit ["reset", "--hard", remoteBranch] p

instance PackageName ClonePackage where
  pkgName = cpName

instance Package ClonePackage where
  pkgLocalVersion = cpLocalVersion
  pkgRemoteVersion = cpRemoteVersion
  pkgInstall = cpInstall
