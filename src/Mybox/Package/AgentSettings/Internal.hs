module Mybox.Package.AgentSettings.Internal where

import Control.Lens qualified as Lens
import Data.Aeson.Lens qualified as Lens
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Yaml qualified as Yaml

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.AgentSettings.Types
import Mybox.Package.Class
import Mybox.Prelude

data AgentSettingsPackage = AgentSettingsPackage
  { name :: Text
  , agents :: NonEmpty Agent
  , read :: [Path AnyAnchor]
  , write :: [Path AnyAnchor]
  , commands :: [CommandRule]
  }
  deriving (Eq, Generic, Show)

mkAgentSettingsPackage :: Text -> NonEmpty Agent -> AgentSettingsPackage
mkAgentSettingsPackage name agents = AgentSettingsPackage{name, agents, read = [], write = [], commands = []}

instance FromJSON AgentSettingsPackage where
  parseJSON = withObjectTotal "AgentSettingsPackage" $ do
    name <- takeField "agent-settings"
    agents <- takeCollapsedNEList "agents"
    readPaths <- takeCollapsedList "read"
    write <- takeCollapsedList "write"
    commands <- takeCollapsedList "commands"
    pure AgentSettingsPackage{read = readPaths, ..}

instance ToJSON AgentSettingsPackage where
  toJSON p =
    object
      [ "agent-settings" .= p.name
      , "agents" .= p.agents
      , "read" .= p.read
      , "write" .= p.write
      , "commands" .= p.commands
      ]

instance PackageName AgentSettingsPackage where
  splitName p = ("agent-settings#" <> p.name, Just p{name = ""})

instance Package AgentSettingsPackage where
  remoteVersion _ = pure "configured"
  localVersion _ = pure $ Just "configured"
  install p = drvAtomic "agent-settings" $ do
    traverse_ (throwLeft . validateCommand) p.commands
    home <- drvHome
    let resolve path = case path.anchor of
          Abs -> mkPath path.text
          Rel -> home <//> path
        readPaths = nubOrd $ map resolve $ p.read <> p.write
        writePaths = nubOrd $ map resolve p.write
    for_ (nubOrd $ toList p.agents) $ \case
      Claude -> installAgent ClaudeConfig home readPaths writePaths p.commands
      Rovo -> installAgent RovoConfig home readPaths writePaths p.commands

installAgent :: (AgentConfig agent, Driver :> es) => agent -> Path Abs -> [Path Abs] -> [Path Abs] -> [CommandRule] -> Eff es ()
installAgent agent home readPaths writePaths commands = do
  let path = home <//> settingsFile agent
      format :: Lens.Prism' ByteString Value
      format = settingsFormat agent
  existing <- ifM (drvIsFile path) (drvReadBinaryFile path) (pure "{}")
  unless (Lens.has format existing) $ throwString $ "Cannot parse agent settings: " <> show path
  updated <- throwLeft $ Lens.traverseOf format (updateSettings agent readPaths writePaths commands) existing
  drvMkdir path.dirname
  drvRun $ shellRaw $ "printf '%s' " <> shellQuote (Text.decodeUtf8 updated) <> " > " <> shellQuote path.text

data PathAccess = ReadAccess | WriteAccess

data ClaudeConfig = ClaudeConfig

data RovoConfig = RovoConfig

class AgentConfig agent where
  settingsFile :: agent -> Path Rel
  settingsFormat :: agent -> Lens.Prism' ByteString Value
  commandSettings :: agent -> Lens.Traversal' Value [Value]
  pathSettings :: agent -> PathAccess -> Lens.Traversal' Value [Value]
  commandRule :: agent -> CommandRule -> Value
  pathRules :: agent -> PathAccess -> Path Abs -> [Value]
  validatePaths :: agent -> [Path Abs] -> [Path Abs] -> Either String ()
  validatePaths _ _ _ = pure ()

instance AgentConfig ClaudeConfig where
  settingsFile _ = ".claude" </> "settings.json"
  settingsFormat _ = Lens._Value
  commandSettings _ = claudeSettings
  pathSettings _ _ = claudeSettings
  commandRule _ rule =
    String $
      "Bash(" <> commandText rule <> case rule of
        CommandPrefix _ -> " *)"
        CommandExact _ -> ")"
  pathRules _ access path =
    [String $ tool <> "(" <> pattern <> ")" | pattern <- pathPatterns ("/" <> path.text)]
   where
    tool = case access of
      ReadAccess -> "Read"
      WriteAccess -> "Edit"

instance AgentConfig RovoConfig where
  settingsFile _ = ".rovo" </> "config.yml"
  settingsFormat _ = Lens.prism' Yaml.encode (either (const Nothing) Just . Yaml.decodeEither')
  commandSettings _ = rovoCommands
  pathSettings _ _ = rovoPaths
  commandRule _ rule = object ["command" .= patternFor, "permission" .= ("allow" :: Text)]
   where
    patternFor =
      "^" <> escapeRegex (commandText rule) <> case rule of
        CommandPrefix _ -> "( .*)?$"
        CommandExact _ -> "$"
  pathRules _ _ path = [String path.text]
  validatePaths _ readPaths writePaths =
    unless (all (\path -> any (`pUnder` path) writePaths) readPaths) $
      Left "Rovo cannot grant read-only external paths; its allowedExternalPaths grants read and write access"

updateSettings :: AgentConfig agent => agent -> [Path Abs] -> [Path Abs] -> [CommandRule] -> Value -> Either String Value
updateSettings agent readPaths writePaths commands config = do
  validatePaths agent readPaths writePaths
  pure
    $ extend (commandSettings agent) (map (commandRule agent) commands)
      . extend (pathSettings agent WriteAccess) (concatMap (pathRules agent WriteAccess) writePaths)
      . extend (pathSettings agent ReadAccess) (concatMap (pathRules agent ReadAccess) readPaths)
    $ config
 where
  extend :: Lens.Traversal' Value [Value] -> [Value] -> Value -> Value
  extend _ [] = id
  extend settings rules = Lens.over settings (nubOrd . (rules <>))

claudeSettings :: Lens.Traversal' Value [Value]
claudeSettings =
  Lens.atKey "permissions"
    . Lens.non (object [])
    . Lens.atKey "allow"
    . Lens.anon (toJSON ([] :: [Value])) (const False)
    . Lens._JSON

rovoCommands :: Lens.Traversal' Value [Value]
rovoCommands =
  Lens.atKey "toolPermissions"
    . Lens.non (object [])
    . Lens.atKey "bash"
    . Lens.non (object [])
    . Lens.atKey "commands"
    . Lens.anon (toJSON ([] :: [Value])) (const False)
    . Lens._JSON

rovoPaths :: Lens.Traversal' Value [Value]
rovoPaths =
  Lens.atKey "toolPermissions"
    . Lens.non (object [])
    . Lens.atKey "allowedExternalPaths"
    . Lens.anon (toJSON ([] :: [Value])) (const False)
    . Lens._JSON

pathPatterns :: Text -> [Text]
pathPatterns path = [path, Text.dropWhileEnd (== '/') path <> "/**"]

escapeRegex :: Text -> Text
escapeRegex = Text.concatMap $ \c ->
  (if c `elem` ("\\.^$|?*+()[]{}" :: String) then "\\" else "") <> Text.singleton c
