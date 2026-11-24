module Mybox.Package.AgentSettings.Types (
  Agent (..),
  agentName,
  CommandMatch (..),
  CommandRule,
  commandMatch,
  commandText,
  mkCommandRule,
) where

import Data.Char (isAlphaNum)
import Data.List.NonEmpty qualified as NonEmpty
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

data CommandMatch = Prefix | Exact
  deriving (Eq, Generic, Ord, Show)

data CommandRule = CommandRule
  { commandMatch :: CommandMatch
  , commandWords :: NonEmpty Text
  }
  deriving (Eq, Ord, Show)

commandText :: CommandRule -> Text
commandText = Text.unwords . toList . commandWords

-- Keep rules literal when translating to wildcard and regex formats.
mkCommandRule :: CommandMatch -> Text -> Either String CommandRule
mkCommandRule match command =
  case NonEmpty.nonEmpty (Text.words command) of
    Just parts
      | command == Text.unwords (toList parts) -> do
          unless (all (Text.all isLiteral) parts) $
            Left "Commands must contain literal words, without quoting, wildcards or shell operators"
          pure $ CommandRule match parts
    _ -> Left "Commands must be nonempty and use single spaces between arguments"
 where
  isLiteral c = isAlphaNum c || c `elem` ("/._-:=+@%," :: String)

instance FromJSON CommandRule where
  parseJSON value = do
    (match, command) <- case value of
      String command -> pure (Prefix, command)
      _ -> withObjectTotal "Exact command" ((Exact,) <$> takeField "exact") value
    either fail pure $ mkCommandRule match command

instance ToJSON CommandRule where
  toJSON rule = case commandMatch rule of
    Prefix -> toJSON $ commandText rule
    Exact -> object ["exact" .= commandText rule]
