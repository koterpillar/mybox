module Mybox.Display.Data where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Prelude

data MDisplay

newtype instance Log MDisplay = MLog {log :: Text}

instance Show (Log MDisplay) where
  show log = Text.unpack log.log

newtype instance Banner MDisplay = MBanner
  { installing :: Set Text
  }
  deriving (Generic)
  deriving (Monoid, Semigroup) via Generically (Banner MDisplay)

instance Show (Banner MDisplay) where
  show banner =
    Text.unpack $
      Text.intercalate "\n" $
        catMaybes
          [ bannerPart "installing" banner.installing
          ]

bannerPart :: Text -> Set Text -> Maybe Text
bannerPart label set
  | Set.null set = Nothing
  | otherwise = Just $ label <> ": " <> Text.intercalate ", " (toList set)

addInstalling :: Text -> Banner MDisplay -> Banner MDisplay
addInstalling text banner = banner{installing = Set.insert text banner.installing}
