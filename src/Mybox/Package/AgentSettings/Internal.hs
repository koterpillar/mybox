module Mybox.Package.AgentSettings.Internal where

import Control.Lens qualified as Lens
import Data.Aeson.Lens qualified as Lens
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
    -- Plan every agent before the first write, including capability checks.
    changes <- settingsPlan p
    for_ changes $ \(path, old, new) -> unless (old == Just new) $ do
      actualPath <- ifM (drvIsSymlink path) (drvRealPath path) (pure path)
      writeSettings actualPath (isJust old) new

writeSettings :: Driver :> es => Path Abs -> Bool -> Text -> Eff es ()
writeSettings path exists content = do
  drvMkdir path.dirname
  bracket
    (mkPath @Abs <$> drvRunOutput ("mktemp" :| [path.text <> ".XXXXXX"]))
    drvRm
    $ \temporary -> do
      when exists $ drvRun $ "cp" :| ["-p", path.text, temporary.text]
      drvRun $ shellRaw $ "printf '%s' " <> shellQuote content <> " > " <> shellQuote temporary.text
      drvRun $ "mv" :| ["-f", temporary.text, path.text]

settingsPlan :: Driver :> es => AgentSettingsPackage -> Eff es [(Path Abs, Maybe Text, Text)]
settingsPlan p = do
  traverse_ (throwLeft . validateCommand) p.commands
  home <- drvHome
  let resolve path = case path.anchor of
        Abs -> mkPath path.text
        Rel -> home <//> path
      readPaths = nubOrd $ map resolve $ p.read <> p.write
      writePaths = nubOrd $ map resolve p.write
  fmap concat $ for (nubOrd $ toList p.agents) $ \agent -> case agent of
    Claude -> jsonPlan (home <//> mkPath @Rel ".claude/settings.json") $ updateClaudeSettings readPaths writePaths p.commands
    Rovo -> do
      let path = home <//> mkPath @Rel ".rovo/config.yml"
      existing <- readSettings path
      config <- maybe (pure $ object []) (yamlDecode @Value) existing
      updated <- throwLeft $ updateRovoSettings readPaths writePaths p.commands config
      let output = Text.decodeUtf8 $ Yaml.encode updated
      pure [(path, existing, if config == updated then fromMaybe output existing else output)]

readSettings :: Driver :> es => Path Abs -> Eff es (Maybe Text)
readSettings path = ifM (drvIsFile path) readExact (pure Nothing)
 where
  -- Driver trims command output. Sentinels preserve significant whitespace
  -- in YAML block strings and keep comparisons with rendered files stable.
  readExact =
    Just . Text.dropEnd 1 . Text.drop 1
      <$> drvRunOutput
        (shellRaw $ "printf '\\001' && cat " <> shellQuote path.text <> " && printf '\\001'")

jsonPlan :: Driver :> es => Path Abs -> (Value -> Value) -> Eff es [(Path Abs, Maybe Text, Text)]
jsonPlan path update = do
  existing <- readSettings path
  config <- maybe (pure $ object []) (jsonDecode @Value "Claude settings") existing
  let updated = update config
      output = jsonEncode updated <> "\n"
  pure [(path, existing, if config == updated then fromMaybe output existing else output)]

data PathAccess = ReadAccess | WriteAccess

data ClaudeConfig = ClaudeConfig

data RovoConfig = RovoConfig

class AgentConfig agent where
  commandSettings :: agent -> Lens.Traversal' Value [Value]
  pathSettings :: agent -> PathAccess -> Lens.Traversal' Value [Value]
  commandRule :: agent -> CommandRule -> Value
  pathRules :: agent -> PathAccess -> Path Abs -> [Value]

instance AgentConfig ClaudeConfig where
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
  commandSettings _ = rovoCommands
  pathSettings _ _ = rovoPaths
  commandRule _ rule = object ["command" .= patternFor, "permission" .= ("allow" :: Text)]
   where
    patternFor =
      "^" <> escapeRegex (commandText rule) <> case rule of
        CommandPrefix _ -> "( .*)?$"
        CommandExact _ -> "$"
  pathRules _ _ path = [String path.text]

updateSettings :: AgentConfig agent => agent -> [Path Abs] -> [Path Abs] -> [CommandRule] -> Value -> Value
updateSettings agent readPaths writePaths commands =
  extend (commandSettings agent) (map (commandRule agent) commands)
    . extend (pathSettings agent WriteAccess) (concatMap (pathRules agent WriteAccess) writePaths)
    . extend (pathSettings agent ReadAccess) (concatMap (pathRules agent ReadAccess) readPaths)
 where
  extend :: Lens.Traversal' Value [Value] -> [Value] -> Value -> Value
  extend _ [] = id
  extend settings rules = Lens.over settings (nubOrd . (<> rules))

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

updateClaudeSettings :: [Path Abs] -> [Path Abs] -> [CommandRule] -> Value -> Value
updateClaudeSettings = updateSettings ClaudeConfig

pathPatterns :: Text -> [Text]
pathPatterns path = [path, Text.dropWhileEnd (== '/') path <> "/**"]

updateRovoSettings :: [Path Abs] -> [Path Abs] -> [CommandRule] -> Value -> Either String Value
updateRovoSettings readPaths writePaths commands config = do
  unless (all (\path -> any (`pUnder` path) writePaths) readPaths) $
    Left "Rovo cannot grant read-only external paths; its allowedExternalPaths grants read and write access"
  unless (null commands) $
    for_ (config Lens.^? rovoCommands) $ \entries ->
      unless (all isAllow entries) $
        Left "Cannot extend Rovo command rules containing explicit ask/deny entries without verified rule precedence"
  pure $ updateSettings RovoConfig readPaths writePaths commands config
 where
  isAllow value = value Lens.^? (Lens.key "permission" . Lens._String) == Just "allow"

escapeRegex :: Text -> Text
escapeRegex = Text.concatMap $ \c ->
  (if c `elem` ("\\.^$|?*+()[]{}" :: String) then "\\" else "") <> Text.singleton c
