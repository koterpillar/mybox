module Mybox.Package.AgentSettings.Codex where

import Data.Char (isAlphaNum)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Toml qualified

import Mybox.Aeson
import Mybox.Package.AgentSettings.Types
import Mybox.Prelude

codexRules :: [CommandRule] -> Either String Text
codexRules rules = Text.unlines <$> traverse render rules
 where
  render (CommandExact _) = Left "Codex does not support exact command rules"
  render (CommandPrefix command) = do
    validateCommand $ CommandPrefix command
    pure $ "prefix_rule(pattern=" <> jsonEncode (Text.words command) <> ", decision=\"allow\")"

mergeCodexRules :: Text -> Text -> Text
mergeCodexRules existing additions =
  case filter (`notElem` Text.lines existing) $ Text.lines additions of
    [] -> existing
    new -> existing <> separator <> Text.unlines new
 where
  separator = if Text.null existing || Text.isSuffixOf "\n" existing then "" else "\n"

-- Modify the active permission profile, preserving all other TOML values.
codexPaths :: [Path Abs] -> [Path Abs] -> Text -> Either String Text
codexPaths readPaths writePaths input = do
  parsed <- first (Text.unpack . Toml.unTomlParseError) $ Toml.parse input
  config <- normalizeToml parsed
  when (HM.member "sandbox_mode" config.tomlPairs || isJust (Toml.lookup "sandbox_workspace_write" config.tomlTables)) $
    Left "Codex path grants require permission profiles; remove legacy sandbox_mode/sandbox_workspace_write settings first"
  selected <- optionalText "default_permissions" config
  let name = fromMaybe ":workspace" selected
  (profileName, configWithProfile) <- case name of
    ":workspace" -> createProfile True config
    ":read-only" -> createProfile False config
    ":danger-full-access" -> Left "Codex has unrestricted access; refusing to replace its permission mode"
    _ -> do
      profiles <- tableAt ["permissions"] config
      unless (isJust $ Toml.lookup (key [name]) profiles.tomlTables) $
        Left "Codex default_permissions names an undefined profile"
      pure (name, config)
  filesystem <- tableAt ["permissions", profileName, "filesystem"] configWithProfile
  inheritedFilesystems <- profileFilesystems [] profileName configWithProfile
  let deniedRoots =
        [ path
        | fs <- inheritedFilesystems
        , (k, Toml.AnyValue (Toml.Text "deny")) <- HM.toList fs.tomlPairs
        , [path] <- [keyParts k]
        ]
  for_ (readPaths <> writePaths) $ \path ->
    when (any (denies path) deniedRoots)
      $ Left
      $ "Codex explicitly denies access to " <> Text.unpack path.text
  updated <- foldM (grant inheritedFilesystems) filesystem $ [(p, "read") | p <- readPaths] <> [(p, "write") | p <- writePaths]
  result <- setTable ["permissions", profileName, "filesystem"] updated configWithProfile
  -- Also validate the printer's output before any settings are written.
  let output = Toml.pretty result
  void $ first (Text.unpack . Toml.unTomlParseError) $ Toml.parse output
  pure output
 where
  denies path denied
    | denied == ":root" = True
    | Text.isPrefixOf "/" denied = mkPath @Abs denied `pUnder` path
    | otherwise = False
  grant inherited fs (path, access) = do
    let pathKey = key [path.text]
    current <- optionalText pathKey fs
    inheritedAccess <- traverse (optionalText pathKey) $ reverse inherited
    let existing = current <|> listToMaybe (catMaybes inheritedAccess)
    when (existing == Just "deny") $ Left $ "Codex explicitly denies " <> Text.unpack path.text
    let permission = if existing == Just "write" then "write" else access
    pure $ Toml.insertKeyVal pathKey (Toml.Text permission) fs

profileFilesystems :: [Text] -> Text -> Toml.TOML -> Either String [Toml.TOML]
profileFilesystems visited name config
  | name `elem` [":workspace", ":read-only"] = pure []
  | name `elem` visited = Left "Codex permission profiles contain an inheritance cycle"
  | otherwise = do
      profiles <- tableAt ["permissions"] config
      unless (isJust $ Toml.lookup (key [name]) profiles.tomlTables) $
        Left "Codex permission profile inherits from an undefined profile"
      profile <- tableAt ["permissions", name] config
      parent <- optionalText "extends" profile
      inherited <- maybe (pure []) (\p -> profileFilesystems (name : visited) p config) parent
      filesystem <- tableAt ["filesystem"] profile
      pure $ inherited <> [filesystem]

key :: [Text] -> Toml.Key
key pieces = case pieces of
  p : ps -> Toml.Key $ piece p :| map piece ps
  [] -> error "Empty TOML key"
 where
  piece value =
    Toml.Piece $
      if not (Text.null value) && Text.all (\c -> isAlphaNum c || c == '_' || c == '-') value
        then value
        else jsonEncode value

keyParts :: Toml.Key -> [Text]
keyParts = map (unquote . Toml.unPiece) . toList . Toml.unKey
 where
  unquote text = case (Text.uncons text, Text.unsnoc text) of
    (Just (q, _), Just (_, q')) | q == q' && q `elem` ("\"'" :: String) -> Text.dropEnd 1 $ Text.drop 1 text
    _ -> text

-- tomland stores dotted sections as prefix-tree keys. Normalize them to
-- nested tables before editing so explicit and implicit parents behave alike.
normalizeToml :: Toml.TOML -> Either String Toml.TOML
normalizeToml input = do
  withPairs <- foldM addPair mempty $ HM.toList input.tomlPairs
  tables <- foldM addTable withPairs $ tableEntries input.tomlTables
  foldM addArray tables $ HM.toList input.tomlTableArrays
 where
  addPair config (k, value) = updateParent (keyParts k) config $ \name parent -> do
    when (HM.member (key [name]) parent.tomlPairs) $ Left "Duplicate Codex settings key"
    pure $ Toml.insertKeyAnyVal (key [name]) value parent
  addTable config (k, value) = do
    normalized <- normalizeToml value
    old <- tableAt (keyParts k) config
    setTable (keyParts k) (old <> normalized) config
  addArray config (k, values) = do
    normalized <- traverse normalizeToml values
    updateParent (keyParts k) config $ \name parent ->
      pure $ Toml.insertTableArrays (key [name]) normalized parent

tableEntries :: Toml.PrefixMap Toml.TOML -> [(Toml.Key, Toml.TOML)]
tableEntries = concatMap treeEntries . HM.elems

treeEntries :: Toml.PrefixTree Toml.TOML -> [(Toml.Key, Toml.TOML)]
treeEntries = \case
  Toml.Leaf k value -> [(k, value)]
  Toml.Branch prefix value children ->
    maybe [] (\v -> [(prefix, v)]) value
      <> [(prefix <> suffix, child) | (suffix, child) <- tableEntries children]

updateParent :: [Text] -> Toml.TOML -> (Text -> Toml.TOML -> Either String Toml.TOML) -> Either String Toml.TOML
updateParent parts config update = case unsnoc parts of
  Nothing -> Left "Empty Codex settings key"
  Just (parents, name) -> do
    parent <- tableAt parents config
    updated <- update name parent
    setTable parents updated config

optionalText :: Toml.Key -> Toml.TOML -> Either String (Maybe Text)
optionalText k config = case HM.lookup k config.tomlPairs of
  Nothing -> pure Nothing
  Just (Toml.AnyValue (Toml.Text value)) -> pure $ Just value
  Just _ -> Left "Expected a string in Codex settings"

tableAt :: [Text] -> Toml.TOML -> Either String Toml.TOML
tableAt [] config = pure config
tableAt (piece : rest) config = do
  let k = key [piece]
  when (HM.member k config.tomlPairs || HM.member k config.tomlTableArrays) $
    Left "Expected a table in Codex settings"
  tableAt rest $ fromMaybe mempty $ Toml.lookup k config.tomlTables

setTable :: [Text] -> Toml.TOML -> Toml.TOML -> Either String Toml.TOML
setTable [] value _ = pure value
setTable (piece : rest) value config = do
  child <- tableAt [piece] config
  updated <- setTable rest value child
  pure $ Toml.insertTable (key [piece]) updated config

createProfile :: Bool -> Toml.TOML -> Either String (Text, Toml.TOML)
createProfile writable config = do
  existing <- tableAt ["permissions", "mybox"] config
  unless (existing == mempty) $ Left "Codex already has an inactive mybox permission profile; select it explicitly or rename it first"
  let parent = if writable then ":workspace" else ":read-only"
      profile = Toml.insertKeyVal "extends" (Toml.Text parent) mempty
  updated <- setTable ["permissions", "mybox"] profile config
  pure ("mybox", Toml.insertKeyVal "default_permissions" (Toml.Text "mybox") updated)
