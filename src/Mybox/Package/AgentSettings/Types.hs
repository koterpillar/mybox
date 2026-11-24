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
import Mybox.JSONCodec
import Mybox.Prelude

data Agent = Claude | Rovo
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving (FromJSON, ToJSON) via ViaJSONCodec Agent

agentName :: Agent -> Text
agentName = \case
  Claude -> "claude"
  Rovo -> "rovo"

instance JSONCodec Agent where
  jsonCodec = enumCodec "agent" agentName

data CommandMatch = Prefix | Exact
  deriving (Eq, Generic, Ord, Show)

data CommandRule = CommandRule
  { commandMatch :: CommandMatch
  , commandWords :: NonEmpty Text
  }
  deriving (Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via ViaJSONCodec CommandRule

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

instance JSONCodec CommandRule where
  jsonCodec =
    ValueCodec
      { to = \rule -> case commandMatch rule of
          Prefix -> String $ commandText rule
          Exact -> exact.to $ commandText rule
      , parse = \value -> do
          (match, command) <- case value of
            String command -> pure (Prefix, command)
            _ -> (Exact,) <$> exact.parse value
          either fail pure $ mkCommandRule match command
      , doc = DocOneOf [DocText, exact.doc]
      }
   where
    exact =
      objectCodec "Exact command" $
        ObjectCodec
          { to = \command -> ["exact" .= (command :: Text)]
          , parse = takeField "exact"
          , doc = [FieldDoc "exact" "Match the entire command and its arguments." True DocText]
          }
