module Mybox.Package.Clone (
  ClonePackage (..),
) where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Destination
import Mybox.Prelude
import Mybox.Tracker
import Mybox.Utils

data ClonePackage = ClonePackage
  { repo :: Text
  , branch :: Maybe Text
  , destination :: Text
  }
  deriving (Eq, Generic, Show)

instance HasField "name" ClonePackage Text where
  getField p = Text.intercalate "#" $ p.repo : toList p.branch

instance FromJSON ClonePackage

instance ToJSON ClonePackage

cpRemote :: ClonePackage -> Text
cpRemote p
  | Text.isPrefixOf "https://" r = r
  | Text.isInfixOf "@" r = r
  | otherwise = "https://github.com/" <> r <> ".git"
 where
  r = p.repo

cpGitArgs :: Driver :> es => [Text] -> ClonePackage -> Eff es (NonEmpty Text)
cpGitArgs args p = do
  dest <- destinationPath p
  pure $ "git" :| (["-C", dest] <> args)

cpRunGit :: Driver :> es => [Text] -> ClonePackage -> Eff es ()
cpRunGit args p = cpGitArgs args p >>= drvRun

cpRevParse ::
  Driver :> es =>
  -- | branch name
  Text ->
  -- | abbrev-ref
  Bool ->
  ClonePackage ->
  Eff es Text
cpRevParse branch abbrevRef p =
  cpGitArgs (join [["rev-parse"], ["--abbrev-ref" | abbrevRef], [branch]]) p
    >>= drvRunOutput

cpLocalVersion :: Driver :> es => ClonePackage -> Eff es (Maybe Text)
cpLocalVersion p = do
  exists <- destinationExists p
  if exists
    then Just <$> cpRevParse "HEAD" False p
    else pure Nothing

cpRemoteVersion :: Driver :> es => ClonePackage -> Eff es Text
cpRemoteVersion p = repoBranchVersion (cpRemote p) p.branch

cpDefaultRemote :: Text
cpDefaultRemote = "origin"

cpRemoteBranch :: Text -> Text
cpRemoteBranch b = cpDefaultRemote <> "/" <> b

cpInstall :: (Driver :> es, PackageTracker :> es) => ClonePackage -> Eff es ()
cpInstall p = do
  destination <- destinationPath p
  exists <- drvIsDir $ destination </> ".git"
  if exists
    then cpRunGit ["remote", "set-url", cpDefaultRemote, cpRemote p] p
    else do
      drvMkdir $ pDirname destination
      drvRm destination
      drvRun $ "git" :| ["clone", cpRemote p, destination]
  remoteBranch <-
    case p.branch of
      Just branch -> pure $ cpRemoteBranch branch
      Nothing -> cpRevParse (cpRemoteBranch "HEAD") True p
  let branch = Text.takeWhileEnd (/= '/') remoteBranch
  cpRunGit ["fetch", "--no-tags", cpDefaultRemote, branch] p
  currentBranch <- cpRevParse "HEAD" True p
  when (currentBranch /= branch) $ cpRunGit ["switch", branch] p
  trkAdd destination
  cpRunGit ["reset", "--hard", remoteBranch] p

instance Package ClonePackage where
  localVersion = cpLocalVersion
  remoteVersion = cpRemoteVersion
  install = cpInstall
