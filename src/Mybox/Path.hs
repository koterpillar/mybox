module Mybox.Path where

import           Data.Function (on)

import           Data.List     (isPrefixOf)

import           Data.Text     (Text)
import qualified Data.Text     as Text

(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b

pDirname :: Text -> Text
pDirname path =
  case Text.splitOn "/" path of
    []  -> "."
    [_] -> "."
    xs  -> Text.intercalate "/" (init xs)

pUnder :: Text -> Text -> Bool
pUnder = isPrefixOf `on` Text.splitOn "/"
