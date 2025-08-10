module Mybox.Aeson (
  module Data.Aeson,
  Parser,
  Pair,
  parseCollapsedList,
  parseCollapsedListMaybe,
  jsonEncode,
  jsonDecode,
  yamlDecode,
  jsonAlternative,
  parseThrow,
  parseWithContext,
  parseJSONWithContext,
) where

import Data.Aeson
import Data.Aeson.Extra
import Data.Aeson.Types
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.String (IsString (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Yaml qualified as Yaml

import Mybox.Prelude

jsonEncode :: ToJSON a => a -> Text
jsonEncode = Text.decodeUtf8 . LBS.toStrict . encode

jsonDecode :: (FromJSON a, HasCallStack, MonadThrow m) => String -> Text -> m a
jsonDecode desc = throwLeft . first augmentError . eitherDecodeStrictText
 where
  augmentError err = "Failed to decode " <> desc <> ": " <> err

yamlDecode :: (FromJSON a, HasCallStack, MonadThrow m) => Text -> m a
yamlDecode = Yaml.decodeThrow . Text.encodeUtf8

jsonAlternative :: Parser a -> Parser a -> Parser a
jsonAlternative p1 p2 = parserCatchError p1 $ \_ err ->
  prependFailure (err <> "; ") p2

parseThrow :: (HasCallStack, MonadThrow m) => (a -> Parser b) -> a -> m b
parseThrow p = throwLeft . parseEither p

parseWithContext :: Text -> (a -> Parser b) -> a -> Parser b
parseWithContext c p v = p v <?> Key (fromString $ Text.unpack c)

parseJSONWithContext :: FromJSON b => Text -> Value -> Parser b
parseJSONWithContext c = parseWithContext c parseJSON

parseCollapsedListMaybe :: FromJSON a => Object -> String -> Parser (Maybe [a])
parseCollapsedListMaybe obj key =
  obj .:? fromString key >>= \case
    Nothing -> pure Nothing
    Just (_ :: Value) -> Just <$> parseCollapsedList obj (fromString key)
