module Mybox.Filters where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Prelude

choose_ :: Show a => [a -> Bool] -> [a] -> a
choose_ fs vs = case choose fs vs of
  Left [] -> error "No candidates to choose from."
  Left cs -> error $ "Cannot choose between: " <> show cs <> "."
  Right v -> v

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
fromSynonyms synonyms key = do
  (key', variants) <- Map.toList synonyms
  let predicate = if key == key' then includes_ else excludes_
  predicate <$> variants

type FilterReqs a =
  ( HasField "prefixes" a [Text]
  , HasField "suffixes" a [Text]
  , HasField "includes" a [Text]
  , HasField "excludes" a [Text]
  )

data FilterFields = FilterFields
  { prefixes :: [Text]
  , suffixes :: [Text]
  , includes :: [Text]
  , excludes :: [Text]
  }

parseFilter :: Object -> Parser FilterFields
parseFilter o = do
  prefixes <- parseCollapsedList o "prefix"
  suffixes <- parseCollapsedList o "suffix"
  includes <- parseCollapsedList o "include"
  excludes <- parseCollapsedList o "exclude"
  pure FilterFields{..}

filterToJSON :: FilterReqs a => a -> [Pair]
filterToJSON a =
  [ "prefix" .= a.prefixes
  , "suffix" .= a.suffixes
  , "include" .= a.includes
  , "exclude" .= a.excludes
  ]

filters :: FilterReqs a => a -> [Text -> Bool]
filters a =
  join
    [ Text.isPrefixOf <$> a.prefixes
    , Text.isSuffixOf <$> a.suffixes
    , includes_ <$> a.includes
    , excludes_ <$> a.excludes
    ]
