module Mybox.Aeson (
  module Data.Aeson,
  Parser,
  Pair,
  parseCollapsedList,
  jsonEncode,
  jsonDecode,
  jsonAlternative,
  parseThrow,
  parseWithContext,
  parseJSONWithContext,
) where

import Control.Exception.Safe (MonadThrow, throwString)
import Data.Aeson
import Data.Aeson.Extra
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as LBS
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)

import Mybox.Utils (throwLeft)

jsonEncode :: ToJSON a => a -> Text
jsonEncode = Text.decodeUtf8 . LBS.toStrict . encode

jsonDecode :: (FromJSON a, HasCallStack, MonadThrow m) => String -> Text -> m a
jsonDecode desc v = case eitherDecodeStrictText v of
  Left err -> throwString $ "Failed to decode " <> desc <> ": " <> err
  Right a -> pure a

jsonAlternative :: Parser a -> Parser a -> Parser a
jsonAlternative p1 p2 = parserCatchError p1 $ \_ err ->
  prependFailure (err <> "; ") p2

parseThrow :: (HasCallStack, MonadThrow m) => (a -> Parser b) -> a -> m b
parseThrow p = throwLeft . parseEither p

parseWithContext :: Text -> (a -> Parser b) -> a -> Parser b
parseWithContext c p v = p v <?> Key (fromString $ Text.unpack c)

parseJSONWithContext :: FromJSON b => Text -> Value -> Parser b
parseJSONWithContext c = parseWithContext c parseJSON
