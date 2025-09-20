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
  parseObjectTotal,
  withObjectTotal,
  ObjectParser,
  takeField,
  takeFieldMaybe,
  takeCollapsedList,
  takeCollapsedListMaybe,
  takeCollapsedNEList,
  CollapsedEither (..),
) where

import Control.Monad.State.Strict
import Data.Aeson hiding (Options)
import Data.Aeson.Extra
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.List (intercalate)
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

type ObjectParser a = StateT Object Parser a

takeField_ :: Key -> (Maybe Value -> Parser a) -> ObjectParser a
takeField_ k f = do
  v <- gets $ KM.lookup k
  case v of
    Nothing -> lift $ f Nothing
    Just _ -> do
      modify $ KM.delete k
      lift $ f v <?> Key k

takeField :: FromJSON a => Key -> ObjectParser a
takeField k = takeField_ k $ \case
  Nothing -> parseFail $ "key " <> show k <> " not found"
  Just v -> parseJSON v

takeFieldMaybe :: FromJSON a => Key -> ObjectParser (Maybe a)
takeFieldMaybe k = takeField_ k $ \case
  Nothing -> pure Nothing
  Just Null -> pure Nothing
  Just v -> Just <$> parseJSON v

takeCollapsedListMaybe :: FromJSON a => Key -> ObjectParser (Maybe [a])
takeCollapsedListMaybe k = fmap getCollapsedList <$> takeFieldMaybe k

takeCollapsedList :: FromJSON a => Key -> ObjectParser [a]
takeCollapsedList k = fromMaybe [] <$> takeCollapsedListMaybe k

takeCollapsedNEList :: FromJSON a => Key -> ObjectParser (NonEmpty a)
takeCollapsedNEList k =
  takeCollapsedList k >>= \case
    [] -> fail $ "Expected non-empty list for key " <> show k
    (x : xs) -> pure (x :| xs)

parseObjectTotal :: ObjectParser a -> Object -> Parser a
parseObjectTotal p o = do
  (r, o') <- runStateT p o
  if KM.null o'
    then pure r
    else parseFail $ "unexpected keys: " <> intercalate ", " (map show $ KM.keys o')

withObjectTotal :: String -> ObjectParser a -> Value -> Parser a
withObjectTotal desc = withObject desc . parseObjectTotal

newtype CollapsedEither a b = CollapsedEither {getCollapsedEither :: Either a b}
  deriving (Eq, Ord, Show)

instance (FromJSON a, FromJSON b) => FromJSON (CollapsedEither a b) where
  parseJSON v = CollapsedEither <$> (jsonAlternative (Right <$> parseJSON v) (Left <$> parseJSON v))

instance (ToJSON a, ToJSON b) => ToJSON (CollapsedEither a b) where
  toJSON (CollapsedEither (Left x)) = toJSON x
  toJSON (CollapsedEither (Right y)) = toJSON y
  toEncoding (CollapsedEither (Left x)) = toEncoding x
  toEncoding (CollapsedEither (Right y)) = toEncoding y
