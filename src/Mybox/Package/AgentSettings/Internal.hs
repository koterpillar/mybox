module Mybox.Package.AgentSettings.Internal where

import Control.Lens qualified as Lens
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Vector qualified as Vector

import Mybox.Aeson
import Mybox.Driver
import Mybox.FileLens qualified as Lens
import Mybox.JSONCodec
import Mybox.Package.AgentSettings.Types
import Mybox.Package.Class
import Mybox.Prelude

data AgentSettingsPackage = AgentSettingsPackage
  { name :: Text
  , agents :: NonEmpty Agent
  , settings :: AgentSettings AnyAnchor
  }
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via ViaJSONCodec AgentSettingsPackage

data AgentSettings anchor = AgentSettings
  { read :: [Path anchor]
  , write :: [Path anchor]
  , execute :: [CommandRule]
  }
  deriving (Eq, Generic, Show)

mapPaths :: (Path a -> Path b) -> AgentSettings a -> AgentSettings b
mapPaths f settings = settings{read = map f settings.read, write = map f settings.write}

mkAgentSettingsPackage :: Text -> NonEmpty Agent -> AgentSettingsPackage
mkAgentSettingsPackage name agents = AgentSettingsPackage{name, agents, settings = AgentSettings [] [] []}

instance JSONCodec AgentSettingsPackage where
  jsonCodec =
    jsonCodecObject "AgentSettingsPackage" $
      field @"name" "agent-settings" "Name of this group of agent permissions."
        :* fieldWith @"agents" "agents" "Agents to configure." arrayCodec
        :* fieldFlat @"settings"
        :* Nil

instance Anchor anchor => JSONObjectCodec (AgentSettings anchor) where
  jsonObjectCodec =
    recordFields $
      fieldWithDefault @"read" "read" "Paths to grant read access to." arrayCodec
        :* fieldWithDefault @"write" "write" "Paths to grant write access to." arrayCodec
        :* fieldWithDefault @"execute" "execute" "Commands to allow the agents to run." arrayCodec
        :* Nil

-- Preserve array output while accepting single values in package definitions.
arrayCodec :: (JSONCodec a, ToJSON a) => ValueCodec a
arrayCodec = case jsonCodec of
  ValueCodec{parse, doc} -> ValueCodec{to = toJSON, parse, doc}

instance PackageName AgentSettingsPackage where
  splitName p = ("agent-settings#" <> p.name, Just p{name = ""})

instance Package AgentSettingsPackage where
  remoteVersion _ = pure "configured"
  localVersion _ = pure $ Just "configured"
  install p = drvAtomic "agent-settings" $ do
    home <- drvHome
    for_ (nubOrd $ toList p.agents) $ \case
      Claude -> installAgent ClaudeConfig home p.settings
      Rovo -> installAgent RovoConfig home p.settings

installAgent :: (AgentConfig agent, Driver :> es) => agent -> Path Abs -> AgentSettings AnyAnchor -> Eff es ()
installAgent agent home settings = do
  throwLeft $ validatePaths agent resolved
  drvModifyBinaryFile (home <//> settingsFile agent) $
    Lens.over (settingsFormat agent) (updateSettings agent resolved)
 where
  resolved = mapPaths resolve settings
  resolve path = case path.anchor of
    Abs -> mkPath path.text
    Rel -> home <//> path

data ClaudeConfig = ClaudeConfig

data RovoConfig = RovoConfig

class AgentConfig agent where
  settingsFile :: agent -> Path Rel
  settingsFormat :: agent -> Lens.Traversal (Maybe ByteString) ByteString Value Value
  commandSettings :: agent -> Lens.Traversal' Value Array
  commandRule :: agent -> CommandRule -> Value
  updatePaths :: agent -> AgentSettings Abs -> Value -> Value
  validatePaths :: agent -> AgentSettings Abs -> Either String ()
  validatePaths _ _ = pure ()

instance AgentConfig ClaudeConfig where
  settingsFile _ = ".claude" </> "settings.json"
  settingsFormat _ = Lens.jsonBytes
  commandSettings _ = claudeSettings
  commandRule _ rule =
    String $
      "Bash(" <> commandText rule <> case commandMatch rule of
        Prefix -> " *)"
        Exact -> ")"
  updatePaths _ settings =
    prependRules claudeSettings $
      concatMap (rules "Edit") settings.write
        <> concatMap (rules "Read") (settings.read <> settings.write)
   where
    rules tool path =
      [String $ tool <> "(" <> pattern <> ")" | pattern <- pathPatterns ("/" <> path.text)]

instance AgentConfig RovoConfig where
  settingsFile _ = ".rovo" </> "config.yml"
  settingsFormat _ = Lens.yamlBytes
  commandSettings _ = rovoCommands
  commandRule _ rule = object ["command" .= patternFor, "permission" .= ("allow" :: Text)]
   where
    patternFor =
      "^" <> escapeRegex (commandText rule) <> case commandMatch rule of
        Prefix -> "( .*)?$"
        Exact -> "$"
  updatePaths _ settings = prependRules rovoPaths $ map (String . (.text)) settings.write
  validatePaths _ settings =
    unless (all (\path -> any (`pUnder` path) settings.write) settings.read) $
      Left "Rovo cannot grant read-only external paths; its allowedExternalPaths grants read and write access"

updateSettings :: AgentConfig agent => agent -> AgentSettings Abs -> Value -> Value
updateSettings agent settings =
  prependRules (commandSettings agent) (map (commandRule agent) settings.execute)
    . updatePaths agent settings

prependRules :: Lens.Traversal' Value Array -> [Value] -> Value -> Value
prependRules _ [] = id
prependRules traversal rules = Lens.over traversal (Vector.fromList rules <>)

claudeSettings :: Lens.Traversal' Value Array
claudeSettings =
  Lens.keyWithDefault "permissions" (Object mempty)
    . Lens.keyWithDefault "allow" (Array mempty)
    . Lens._Array

rovoCommands :: Lens.Traversal' Value Array
rovoCommands =
  Lens.keyWithDefault "toolPermissions" (Object mempty)
    . Lens.keyWithDefault "bash" (Object mempty)
    . Lens.keyWithDefault "commands" (Array mempty)
    . Lens._Array

rovoPaths :: Lens.Traversal' Value Array
rovoPaths =
  Lens.keyWithDefault "toolPermissions" (Object mempty)
    . Lens.keyWithDefault "allowedExternalPaths" (Array mempty)
    . Lens._Array

pathPatterns :: Text -> [Text]
pathPatterns path = [path, Text.dropWhileEnd (== '/') path <> "/**"]

escapeRegex :: Text -> Text
escapeRegex = Text.concatMap $ \c ->
  (if c `elem` ("\\.^$|?*+()[]{}" :: String) then "\\" else "") <> Text.singleton c
