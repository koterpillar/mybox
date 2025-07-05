module Mybox.Aeson (ToJSON (..), FromJSON (..), genericToEncoding, defaultOptions, jsonEncode, jsonDecode) where

import Control.Exception.Safe (MonadThrow, throwString)
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

jsonEncode :: ToJSON a => a -> Text
jsonEncode = Text.decodeUtf8 . LBS.toStrict . encode

jsonDecode :: (FromJSON a, MonadThrow m) => String -> Text -> m a
jsonDecode desc v = case eitherDecodeStrictText v of
  Left err -> throwString $ "Failed to decode " <> desc <> ": " <> err
  Right a -> pure a
