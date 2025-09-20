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
  parseJSON = withObjectTotal "Match" $ do
    host <- takeCollapsedListMaybe "host"
    component <- takeCollapsedList "component"
    pure $ Match{..}
