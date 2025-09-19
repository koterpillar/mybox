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

data instance Banner MDisplay = MBanner
  { checking :: Set Text
  , installing :: Set Text
  }
  deriving (Eq, Generic)
  deriving (Monoid, Semigroup) via Generically (Banner MDisplay)

instance TerminalShow (Banner MDisplay) where
  terminalShow banner =
    catMaybes
      [ bannerPart Blue "checking" banner.checking
      , bannerPart Green "installing" banner.installing
      ]

bannerPart :: Color -> Text -> Set Text -> Maybe [TerminalItem]
bannerPart color label set
  | Set.null set = Nothing
  | otherwise =
      Just $
        (tiMk label){foreground = Just color}
          : tiSpace
          : intersperse tiComma (map tiMk (toList set))

bannerChecking :: Text -> Banner MDisplay
bannerChecking text = mempty{checking = Set.singleton text}

bannerInstalling :: Text -> Banner MDisplay
bannerInstalling text = mempty{installing = Set.singleton text}
