module Mybox.Utils where

import Data.Text qualified as Text
import System.FilePath.Glob (compile, match)

import Mybox.Prelude

glob :: Text -> Text -> Bool
glob pattern text = match (compile $ Text.unpack pattern) (Text.unpack text)
