module Mybox.Config.Match (
  Match (..),
) where

import Mybox.Aeson
import Mybox.Prelude

data Match = Match
  { host :: Maybe [Text]
  , component :: [Text]
  }
  deriving (Generic, Show)

instance FromJSON Match where
  parseJSON = withObject "Match" $ \obj -> do
    host <-
      obj .:? "host" >>= \case
        Nothing -> pure Nothing
        Just (_ :: Value) -> Just <$> parseCollapsedList obj "host"
    component <- parseCollapsedList obj "component"
    pure $ Match{..}
