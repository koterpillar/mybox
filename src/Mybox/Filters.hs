module Mybox.Filters where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Prelude

choose_ :: Show a => [a -> Bool] -> [a] -> Either String a
choose_ fs vs = case choose fs vs of
  Left [] -> Left "No candidates to choose from."
  Left cs -> Left $ "Cannot choose between: " <> show cs <> "."
  Right v -> Right v

choose :: Show a => [a -> Bool] -> [a] -> Either [a] a
choose _ [] = Left []
choose _ [x] = Right x
choose [] vs = Left vs
choose (f : fs) vs = case filter f vs of
  [] -> choose fs vs
  vs' -> choose fs vs'

includes_ :: Text -> Text -> Bool
includes_ x y = Text.isInfixOf x $ Text.toLower y

excludes_ :: Text -> Text -> Bool
excludes_ x y = not $ Text.isInfixOf x $ Text.toLower y

fromSynonyms :: Ord k => Map k [Text] -> k -> [Text -> Bool]
fromSynonyms synonyms key = positive <> negative
 where
  positive = fmap includes_ $ fromMaybe [] $ Map.lookup key synonyms
  negative =
    [ excludes_ variant
    | (key', variants) <- Map.toList synonyms
    , key /= key'
    , variant <- variants
    ]

data FilterFields = FilterFields
  { prefixes :: [Text]
  , suffixes :: [Text]
  , includes :: [Text]
  , excludes :: [Text]
  }
  deriving (Eq, Generic, Ord, Show)
  deriving (Monoid, Semigroup) via Generically FilterFields

instance HasEmpty FilterFields where
  emptyValue = mempty

takeFilter :: ObjectParser FilterFields
takeFilter = do
  prefixes <- takeCollapsedList "prefix"
  suffixes <- takeCollapsedList "suffix"
  includes <- takeCollapsedList "include"
  excludes <- takeCollapsedList "exclude"
  pure FilterFields{..}

parseFilter :: Object -> Parser FilterFields
parseFilter = parseObjectTotal takeFilter

filterToJSON :: FilterFields -> [Pair]
filterToJSON a =
  [ "prefix" .= a.prefixes
  , "suffix" .= a.suffixes
  , "include" .= a.includes
  , "exclude" .= a.excludes
  ]

toFilters :: FilterFields -> [Text -> Bool]
toFilters a =
  join
    [ Text.isPrefixOf <$> a.prefixes
    , Text.isSuffixOf <$> a.suffixes
    , includes_ <$> a.includes
    , excludes_ <$> a.excludes
    ]
