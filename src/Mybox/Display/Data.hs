module Mybox.Display.Data where

import Data.List (intersperse)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Prelude

data MDisplay

newtype instance Log MDisplay = MLog {log :: Text}

instance TerminalShow (Log MDisplay) where
  terminalShow _ (MLog log) = [[tiMk log]]

data instance Banner MDisplay = MBanner
  { all :: Set Text
  , checking :: Set Text
  , installing :: Set Text
  , unchanged :: Set Text
  , modified :: Set Text
  }
  deriving (Eq, Generic)
  deriving (Monoid, Semigroup) via Generically (Banner MDisplay)

instance TerminalShow (Banner MDisplay) where
  terminalShow width banner =
    wrapLines width $
      catMaybes
        [ bannerPart Magenta "checking" banner.checking
        , bannerPart Blue "installing" banner.installing
        , progressPart width banner
        , bannerPart Green "installed" banner.modified
        ]

bannerPart :: Color -> Text -> Set Text -> Maybe TerminalLine
bannerPart color label set
  | Set.null set = Nothing
  | otherwise =
      Just $
        (tiMk label){foreground = Just color}
          : tiSpace
          : intersperse tiComma (map tiMk (toList set))

progressPart :: Maybe Int -> Banner MDisplay -> Maybe TerminalLine
progressPart width banner = makeProgressBar width finishedCount totalCount
 where
  totalCount = Set.size banner.all
  finishedCount = Set.size banner.unchanged + Set.size banner.modified

makeProgressBar :: Maybe Int -> Int -> Int -> Maybe TerminalLine
makeProgressBar width progress total
  | total == 0 = Nothing
  | progress >= total = Nothing
  | otherwise = Just [tiMk $ filledPart <> emptyPart, tiSpace, tiNumber progress, tiMk "/", tiNumber total]
 where
  -- Reserve space for the numbers
  availableWidth = max 10 (fromMaybe 80 width - 10)
  filledChars = (progress * availableWidth) `div` total
  emptyChars = availableWidth - filledChars
  filledPart = Text.replicate filledChars "#"
  emptyPart = Text.replicate emptyChars " "
  tiNumber = tiMk . Text.pack . show

bannerPending :: Text -> Banner MDisplay
bannerPending text = mempty{all = Set.singleton text}

bannerChecking :: Text -> Banner MDisplay
bannerChecking text = mempty{checking = Set.singleton text}

bannerInstalling :: Text -> Banner MDisplay
bannerInstalling text = mempty{installing = Set.singleton text}

bannerUnchanged :: Text -> Banner MDisplay
bannerUnchanged text = mempty{unchanged = Set.singleton text}

bannerModified :: Text -> Banner MDisplay
bannerModified text = mempty{modified = Set.singleton text}
