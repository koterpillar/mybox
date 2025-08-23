module Mybox.Display.Data where

import Data.List (intersperse)
import Data.Set qualified as Set
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Prelude

data MDisplay

newtype instance Log MDisplay = MLog {log :: Text}

instance TerminalShow (Log MDisplay) where
  terminalShow (MLog log) = [[tiMk log]]

instance Show (Log MDisplay) where
  show = dumbShow

newtype instance Banner MDisplay = MBanner
  { installing :: Set Text
  }
  deriving (Generic)
  deriving (Monoid, Semigroup) via Generically (Banner MDisplay)

instance TerminalShow (Banner MDisplay) where
  terminalShow banner = catMaybes [bannerPart Green "installing" banner.installing]

bannerPart :: Color -> Text -> Set Text -> Maybe [TerminalItem]
bannerPart color label set
  | Set.null set = Nothing
  | otherwise =
      Just $
        (tiMk label){foreground = Just color}
          : tiSpace
          : intersperse tiComma (map tiMk (toList set))

instance Show (Banner MDisplay) where
  show = dumbShow

addInstalling :: Text -> Banner MDisplay -> Banner MDisplay
addInstalling text banner = banner{installing = Set.insert text banner.installing}
