module Mybox.Display.Data where

import Data.Text qualified as Text
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Prelude

data MDisplay

newtype instance Log MDisplay = MLog {log :: Text}

instance Show (Log MDisplay) where
  show log = Text.unpack log.log

newtype instance Banner MDisplay = MBanner
  { installing :: [Text]
  }
  deriving (Generic)
  deriving (Monoid, Semigroup) via Generically (Banner MDisplay)

instance Show (Banner MDisplay) where
  show banner =
    Text.unpack $
      Text.unlines
        [ "installing: " <> Text.unwords banner.installing
        ]

addInstalling :: Text -> Banner MDisplay -> Banner MDisplay
addInstalling text banner = banner{installing = text : banner.installing}
