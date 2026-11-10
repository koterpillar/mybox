module Mybox.Package.AgentSettings.Internal (
  module Mybox.Package.AgentSettings.Types,
  AgentSettingsPackage (..),
  mkAgentSettingsPackage,
  settingsPlan,
  claudeSettings,
  kiloSettings,
  rovoSettings,
) where

import Data.List (nub)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Yaml qualified as Yaml

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.AgentSettings.Codex
import Mybox.Package.AgentSettings.Config
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
    when (Text.null $ Text.strip name) $ fail "Agent settings packages require a nonempty name"
    agents <- takeCollapsedNEList "agents"
    readPaths <- takeCollapsedList "read"
    write <- takeCollapsedList "write"
    commands <- takeCollapsedList "commands"
    traverse_ (either fail pure . validatePath) $ readPaths <> write
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

-- Do not track shared settings files: removing this package must not delete
-- the user's settings for an agent.
instance Package AgentSettingsPackage where
  remoteVersion _ = pure "configured"
  localVersion p = do
    changes <- settingsPlan p
    pure $ if all (\(_, old, new) -> old == Just new) changes then Just "configured" else Nothing
  install p = drvAtomic "agent-settings" $ do
    -- Plan every agent before the first write, including capability checks.
    changes <- settingsPlan p
    for_ changes $ \(path, old, new) -> unless (old == Just new) $ do
      actualPath <- ifM (drvIsSymlink path) (drvRealPath path) (pure path)
      for_ old $ \_ -> do
        let backup = mkPath @Abs $ actualPath.text <> ".mybox-backup"
        unlessM (drvIsFile backup) $ drvRun $ "cp" :| ["-p", actualPath.text, backup.text]
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
      readPaths = nub $ map resolve $ p.read <> p.write
      writePaths = nub $ map resolve p.write
  traverse_ (throwLeft . validatePath) $ p.read <> p.write
  fmap concat $ for (nub $ toList p.agents) $ \agent -> case agent of
    Claude -> jsonPlan (home <//> mkPath @Rel ".claude/settings.json") $ claudeSettings readPaths writePaths p.commands
    Kilo -> do
      configHome <- fromMaybe (home <//> mkPath @Rel ".config") . fmap (mkPath @Abs) <$> drvEnv "XDG_CONFIG_HOME"
      let jsonc = configHome <//> mkPath @Rel "kilo/kilo.jsonc"
          json = configHome <//> mkPath @Rel "kilo/kilo.json"
      path <- ifM (drvIsFile jsonc) (pure jsonc) $ ifM (drvIsFile json) (pure json) (pure jsonc)
      jsonPlan path $ kiloSettings readPaths writePaths p.commands
    Rovo -> do
      let current = home <//> mkPath @Rel ".rovodev/config.yml"
          legacy = home <//> mkPath @Rel ".rovo/config.yml"
      path <- ifM (drvIsFile current) (pure current) $ ifM (drvIsFile legacy) (pure legacy) (pure current)
      existing <- readSettings path
      config <- maybe (pure emptyConfig) (fmap fromValue . yamlDecode @Value) existing
      updated <- throwLeft $ rovoSettings readPaths writePaths p.commands config
      let output = Text.decodeUtf8 $ Yaml.encode $ toValue updated
      pure [(path, existing, if config == updated then fromMaybe output existing else output)]
    Codex -> do
      codexHome <- fromMaybe (home </> ".codex") . fmap (mkPath @Abs) <$> drvEnv "CODEX_HOME"
      rules <- throwLeft $ codexRules p.commands
      ruleChanges <-
        if null p.commands
          then pure []
          else do
            let path = codexHome <//> mkPath @Rel "rules/mybox.rules"
            existing <- readSettings path
            let output = mergeCodexRules (fromMaybe "" existing) rules
            pure [(path, existing, output)]
      pathChanges <-
        if null readPaths && null writePaths
          then pure []
          else do
            let path = codexHome </> "config.toml"
            existing <- readSettings path
            updated <- throwLeft $ codexPaths readPaths writePaths $ fromMaybe "" existing
            pure [(path, existing, updated)]
      pure $ ruleChanges <> pathChanges

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM predicate yes no = predicate >>= \b -> if b then yes else no

readSettings :: Driver :> es => Path Abs -> Eff es (Maybe Text)
readSettings path = ifM (drvIsFile path) readExact (pure Nothing)
 where
  -- Driver trims command output. Sentinels preserve significant whitespace
  -- in YAML block strings and keep comparisons with rendered files stable.
  readExact =
    Just . Text.dropEnd 1 . Text.drop 1
      <$> drvRunOutput
        (shellRaw $ "printf '\\001' && cat " <> shellQuote path.text <> " && printf '\\001'")

jsonPlan :: Driver :> es => Path Abs -> (Config -> Either String Config) -> Eff es [(Path Abs, Maybe Text, Text)]
jsonPlan path update = do
  existing <- readSettings path
  config <- throwLeft $ maybe (pure emptyConfig) parseConfig existing
  updated <- throwLeft $ update config
  let output = renderConfig updated <> "\n"
  pure [(path, existing, if config == updated then fromMaybe output existing else output)]

validatePath :: Path AnyAnchor -> Either String ()
validatePath path = do
  when (".." `elem` path.segments) $ Left "Agent settings paths cannot contain '..'"
  when (Text.any (`elem` ("*?[]{}()!\\\n\r\t$~\"'" :: String)) path.text) $
    Left "Agent settings paths must be literal paths, without patterns or expansions"

claudeSettings :: [Path Abs] -> [Path Abs] -> [CommandRule] -> Config -> Either String Config
claudeSettings readPaths writePaths commands =
  modifyConfig ["permissions", "allow"]
    $ appendItems
    $ map textConfig
    $ ["Read(" <> pattern <> ")" | path <- readPaths, pattern <- pathPatterns ("/" <> path.text)]
      <> ["Edit(" <> pattern <> ")" | path <- writePaths, pattern <- pathPatterns ("/" <> path.text)]
      <> map (\rule -> "Bash(" <> commandText rule <> case rule of CommandPrefix _ -> " *)"; CommandExact _ -> ")") commands

pathPatterns :: Text -> [Text]
pathPatterns path = [path, Text.dropWhileEnd (== '/') path <> "/**"]

kiloSettings :: [Path Abs] -> [Path Abs] -> [CommandRule] -> Config -> Either String Config
kiloSettings readPaths writePaths commands config = do
  unless (null readPaths && null writePaths) $
    Left "Kilo cannot represent global read/write grants for fixed paths: file rules are relative to each active worktree"
  if null commands
    then pure config
    else do
      checked <- modifyConfig ["permission"] checkPermissions config
      modifyConfig ["permission", "bash"] (allowPatterns $ map commandPattern commands) checked
 where
  checkPermissions value@(Fields fields) = do
    when (lookup "*" fields == Just (textConfig "deny")) $
      Left "Cannot extend Kilo commands with an explicit denial of all tools"
    pure value
  checkPermissions _ = Left "Expected Kilo permissions to be an object"
  commandPattern (CommandPrefix command) = command <> " *"
  commandPattern (CommandExact command) = command

rovoSettings :: [Path Abs] -> [Path Abs] -> [CommandRule] -> Config -> Either String Config
rovoSettings readPaths writePaths commands config = do
  unless (all (\path -> any (`pUnder` path) writePaths) readPaths) $
    Left "Rovo cannot grant read-only external paths; its allowedExternalPaths grants read and write access"
  withPaths <-
    if null writePaths
      then pure config
      else
        modifyConfig ["toolPermissions", "allowedExternalPaths"] (appendItems $ map (textConfig . (.text)) writePaths) config
  if null commands
    then pure withPaths
    else
      modifyConfig ["toolPermissions", "bash", "commands"] prependCommands withPaths
 where
  newCommands = map (\rule -> Fields [("command", textConfig $ patternFor rule), ("permission", textConfig "allow")]) commands
  -- Do not assume precedence between overlapping Rovo regex rules.
  prependCommands = \case
    Fields [] -> pure $ Items $ nub newCommands
    Items existing -> do
      unless (all isAllow existing) $
        Left "Cannot extend Rovo command rules containing explicit ask/deny entries without verified rule precedence"
      pure $ Items $ nub $ existing <> newCommands
    _ -> Left "Expected Rovo bash commands to be a list"
  isAllow (Fields fields) = lookup "permission" fields == Just (textConfig "allow")
  isAllow _ = False
  patternFor rule =
    "^" <> escapeRegex (commandText rule) <> case rule of
      CommandPrefix _ -> "( .*)?$"
      CommandExact _ -> "$"

escapeRegex :: Text -> Text
escapeRegex = Text.concatMap $ \c ->
  (if c `elem` ("\\.^$|?*+()[]{}" :: String) then "\\" else "") <> Text.singleton c
