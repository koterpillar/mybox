module Mybox.FileLens (
  module Data.Aeson.Lens,
  jsonBytes,
  yamlBytes,
  keyWithDefault,
) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Yaml qualified as Yaml

-- | Focus on valid JSON, using an empty object for a missing file.
jsonBytes :: Traversal (Maybe ByteString) ByteString Value Value
jsonBytes = valueBytes _Value

-- | Focus on valid YAML, using an empty object for a missing file.
-- Changed values are re-encoded without the original formatting or comments.
yamlBytes :: Traversal (Maybe ByteString) ByteString Value Value
yamlBytes = valueBytes $ prism' Yaml.encode (either (const Nothing) Just . Yaml.decodeEither')

valueBytes :: Prism' ByteString Value -> Traversal (Maybe ByteString) ByteString Value Value
valueBytes codec f Nothing = review codec <$> f (Object mempty)
valueBytes codec f (Just bytes) = case matching codec bytes of
  Left _ -> pure bytes
  Right value -> fmap (\updated -> if updated == value then bytes else review codec updated) (f value)

-- | Focus on an object key, inserting it only if an update changes the default.
keyWithDefault :: Key -> Value -> Traversal' Value Value
keyWithDefault name fallback f = atKey name update
 where
  update Nothing = fmap (\value -> if value == fallback then Nothing else Just value) (f fallback)
  update (Just value) = Just <$> f value
