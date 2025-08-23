module Mybox.Display.Data where

import Data.Set qualified as Set
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Prelude

data MDisplay

newtype instance Log MDisplay = MLog {log :: Text}

instance TerminalShow (Log MDisplay) where
  terminalShow (MLog log) = [[mkTerminalItem log]]

instance Show (Log MDisplay) where
  show = dumbShow

newtype instance Banner MDisplay = MBanner
  { installing :: Set Text
  }
  deriving (Generic)
  deriving (Monoid, Semigroup) via Generically (Banner MDisplay)

instance TerminalShow (Banner MDisplay) where
  terminalShow banner = catMaybes [bannerPart "installing" banner.installing]

bannerPart :: Text -> Set Text -> Maybe [TerminalItem]
bannerPart label set
  | Set.null set = Nothing
  | otherwise = Just $ mkTerminalItem label : map mkTerminalItem (toList set)

instance Show (Banner MDisplay) where
  show = dumbShow

addInstalling :: Text -> Banner MDisplay -> Banner MDisplay
addInstalling text banner = banner{installing = Set.insert text banner.installing}
