module Mybox.Aeson (
  module Data.Aeson,
  Parser,
  Pair,
  parseCollapsedList,
  jsonEncode,
  jsonDecode,
) where

import Control.Exception.Safe (MonadThrow, throwString)
import Data.Aeson
import Data.Aeson.Extra
import Data.Aeson.Types (Pair, Parser)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)

jsonEncode :: ToJSON a => a -> Text
jsonEncode = Text.decodeUtf8 . LBS.toStrict . encode

jsonDecode :: (FromJSON a, HasCallStack, MonadThrow m) => String -> Text -> m a
jsonDecode desc v = case eitherDecodeStrictText v of
  Left err -> throwString $ "Failed to decode " <> desc <> ": " <> err
  Right a -> pure a
