module Mybox.Aeson (jsonEncode, jsonDecode) where

import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

jsonEncode :: ToJSON a => a -> Text
jsonEncode = Text.decodeUtf8 . LBS.toStrict . encode

jsonDecode :: FromJSON a => Text -> Maybe a
jsonDecode = decodeStrictText
