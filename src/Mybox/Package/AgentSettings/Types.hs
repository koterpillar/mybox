module Mybox.Package.AgentSettings.Types where

import Data.Char (isAlphaNum)
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Prelude

data Agent = Claude | Rovo
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

agentName :: Agent -> Text
agentName = \case
  Claude -> "claude"
  Rovo -> "rovo"

instance ToJSON Agent where
  toJSON = toJSON . agentName

instance FromJSON Agent where
  parseJSON = withText "Agent" $ \name ->
    maybe (fail $ "Unknown agent: " <> Text.unpack name) pure $
      find ((== name) . agentName) [minBound .. maxBound]

data CommandRule = CommandPrefix Text | CommandExact Text
  deriving (Eq, Generic, Ord, Show)

commandText :: CommandRule -> Text
commandText = \case
  CommandPrefix command -> command
  CommandExact command -> command

-- Keep rules literal when translating to wildcard and regex formats.
validateCommand :: CommandRule -> Either String ()
validateCommand rule = do
  let command = commandText rule
  unless (not (Text.null command) && command == Text.unwords (Text.words command)) $
    Left "Commands must be nonempty and use single spaces between arguments"
  unless (Text.all (\c -> isAlphaNum c || c `elem` (" /._-:=+@%," :: String)) command) $
    Left "Commands must contain literal words, without quoting, wildcards or shell operators"

instance FromJSON CommandRule where
  parseJSON value = do
    rule <- case value of
      String command -> pure $ CommandPrefix command
      _ -> withObjectTotal "Exact command" (CommandExact <$> takeField "exact") value
    either fail pure $ validateCommand rule
    pure rule

instance ToJSON CommandRule where
  toJSON = \case
    CommandPrefix command -> toJSON command
    CommandExact command -> object ["exact" .= command]
