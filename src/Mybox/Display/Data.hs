module Mybox.Display.Data where

import Data.List (intersperse)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Display.Ops
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
  , errors :: Map Text Text
  }
  deriving (Eq, Generic)
  deriving (Monoid, Semigroup) via Generically (Banner MDisplay)

instance TerminalShow (Banner MDisplay) where
  terminalShow width banner =
    tiWrapLines width $
      join
        [ bannerPart Green "installed" banner.modified
        , progressPart width banner
        , bannerPart Blue "installing" banner.installing
        , bannerPart Magenta "checking" banner.checking
        , errorsPart $ Map.toList banner.errors
        ]

bannerPart :: Color -> Text -> Set Text -> [TerminalLine]
bannerPart color label set
  | Set.null set = []
  | otherwise =
      [ (tiMk label){foreground = Just color}
          : tiSpace
          : intersperse tiComma (map tiMk (toList set))
      ]

progressPart :: Maybe Int -> Banner MDisplay -> [TerminalLine]
progressPart width banner = toList $ tiProgressBar width finishedCount totalCount
 where
  totalCount = Set.size banner.all
  finishedCount = Set.size banner.unchanged + Set.size banner.modified

errorsPart :: [(Text, Text)] -> [TerminalLine]
errorsPart = map $ uncurry errorPart
 where
  errorPart pkg msg = [label, tiSpace, tiMk pkg, tiColon, tiMk msg]
  label = (tiMk "error"){foreground = Just Red}

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

bannerFailed :: Text -> SomeException -> Banner MDisplay
bannerFailed text err =
  mempty{errors = Map.singleton text (Text.pack $ displayException err)}
