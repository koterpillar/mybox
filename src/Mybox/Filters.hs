module Mybox.Filters where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

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
