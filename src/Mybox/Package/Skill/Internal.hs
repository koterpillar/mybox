module Mybox.Package.Skill.Internal where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.NPM
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Tracker
import Mybox.Utils

-- | Agent skills installed with the @skills@ CLI.
data SkillPackage = SkillPackage
  { source :: Text
  -- ^ A GitHub shortcut (@owner/repo@), or a base URL publishing
  -- @.well-known/skills/index.json@
  , only :: Maybe [Text]
  -- ^ Skills to install, empty for all the source provides
  , agents :: [Text]
  -- ^ Agents to install the skills for, in addition to the universal
  -- directory.
  }
  deriving (Eq, Generic, Show)

mkSkillPackage :: Text -> SkillPackage
mkSkillPackage source = SkillPackage{source, only = Nothing, agents = []}

instance PackageName SkillPackage where
  splitName = genericSplitName' @'["skills"] @'["source"]

instance FromJSON SkillPackage where
  parseJSON = withObjectTotal "SkillPackage" $ do
    source <- takeField "skill"
    only <- takeCollapsedListMaybe "only"
    agents <- takeCollapsedList "agents"
    pure SkillPackage{..}

instance ToJSON SkillPackage where
  toJSON p =
    object $
      ["skill" .= p.source]
        <> ["only" .= p.only | isJust p.only]
        <> ["agents" .= p.agents | not (null p.agents)]

prerequisites :: App es => SkillPackage -> Eff es ()
prerequisites p = do
  queueInstall $ mkNPMPackage "skills"
  when (isGithubShortcut p.source) $ queueInstall $ mkSystemPackage "git"

indexUrl :: SkillPackage -> Text
indexUrl p =
  Text.dropWhileEnd (== '/') p.source <> "/.well-known/skills/index.json"

skillRemoteVersion :: App es => SkillPackage -> Eff es Text
skillRemoteVersion p
  | isGithubShortcut p.source = do
      prerequisites p
      drvRepoBranchVersion (normalizeGitRepoUrl p.source) Nothing
  | otherwise = drvUrlVersion $ indexUrl p

skillAddArgs :: SkillPackage -> Args
skillAddArgs p =
  "skills"
    :| ["add", p.source]
    <> ("--skill" : (fromMaybe ["*"] p.only))
    <> ("--agent" : "universal" : p.agents)
    <> ["--global", "--yes", "--copy"]

installedPaths :: Text -> [Path Rel]
installedPaths = map mkPath . mapMaybe (Text.stripPrefix "~/") . Text.words

skillInstall :: App es => SkillPackage -> Eff es ()
skillInstall p = do
  prerequisites p
  output <- drvRunOutput $ skillAddArgs p
  home <- drvHome
  for_ (installedPaths output) $ trkAdd p . (home <//>)

instance Package SkillPackage where
  localVersion = manualVersion
  remoteVersion = skillRemoteVersion
  install = manualVersionInstall skillInstall
