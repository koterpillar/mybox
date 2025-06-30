module Mybox.Path where

import Data.Function (on)
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text

(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b

pDirname :: Text -> Text
pDirname path =
  case Text.splitOn "/" path of
    [] -> "."
    [_] -> "."
    xs -> Text.intercalate "/" (init xs)

pFilename :: Text -> Maybe Text
pFilename path = listToMaybe $ reverse $ Text.splitOn "/" path

pUnder :: Text -> Text -> Bool
pUnder = isPrefixOf `on` Text.splitOn "/"
