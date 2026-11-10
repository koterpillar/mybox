module Mybox.Package.AgentSettings.Codex where

import Data.Text qualified as Text

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
