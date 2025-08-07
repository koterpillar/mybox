module Mybox.Utils where

import Control.Exception.Safe (MonadThrow, throwString)
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath.Glob (compile, match)

glob :: Text -> Text -> Bool
glob pattern text = match (compile $ Text.unpack pattern) (Text.unpack text)

throwLeft :: MonadThrow m => Either String a -> m a
throwLeft = either throwString pure
