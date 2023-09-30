module Process
  ( procText
  , procDecode
  , procEncode
  ) where

import qualified Data.ByteString.Lazy as LBS

import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text

import           System.Process.Typed hiding (Inherit)

import           Driver.Types

procText :: NonEmpty Text -> ProcessConfig () () ()
procText (cmd :| args) = proc (Text.unpack cmd) (map Text.unpack args)

procDecode :: LBS.ByteString -> Text
procDecode = Text.strip . Text.decodeUtf8 . LBS.toStrict

procEncode :: Text -> LBS.ByteString
procEncode = LBS.fromStrict . Text.encodeUtf8
