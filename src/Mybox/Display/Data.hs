module Mybox.Display.Data where

import Data.Aeson.Key qualified as Key
import Data.List (intercalate, intersperse)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Version
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Display.Ops
import Mybox.JSONCodec
import Mybox.Prelude

data MDisplay

data instance Log MDisplay
  = MLog Text
  | MVersion Version
  | MDoc ValueDoc

instance TerminalShow (Log MDisplay) where
  terminalShow _ (MLog log) = [[tiMk log]]
  terminalShow _ (MVersion version) =
    [
      [ tiMk "mybox"
      , tiSpace
      , tiMk $ Text.pack $ showVersion version
      ]
    ]
  terminalShow width (MDoc doc) = tiWrapLines width $ docLines doc

-- | Render documentation of a JSON value, one section per object shape.
docLines :: ValueDoc -> [TerminalLine]
docLines (DocOneOf docs) = intercalate [[]] $ map docLines docs
docLines (DocObject name fields) = [(tiMk name){foreground = Just Green}] : fieldLines 1 fields
docLines doc = [[tiMk $ docType doc]]

fieldLines :: Int -> [FieldDoc] -> [TerminalLine]
fieldLines indent fields = concatMap fieldLine fields
 where
  fieldLine f =
    [ tiMk $ Text.replicate (indent * 2) " "
    , tiMk $ if f.required then "* " else "  "
    , (tiMk $ pad keyWidth $ Key.toText f.key){foreground = Just Blue}
    , tiMk "  "
    , tiMk $ pad typeWidth $ docType f.value
    , tiMk "  "
    , tiMk f.description
    ]
      : nested f.value
  nested (DocObject _ inner) = fieldLines (succ indent) inner
  nested _ = []
  keyWidth = width $ Key.toText . (.key)
  typeWidth = width $ docType . (.value)
  width f = maximum $ 0 : map (Text.length . f) fields
  pad n = Text.justifyLeft n ' '

-- | Short description of the type of a JSON value.
docType :: ValueDoc -> Text
docType DocText = "text"
docType DocBool = "boolean"
docType (DocEnum values) = Text.intercalate " | " values
docType (DocList doc) = "[" <> docType doc <> "]"
docType (DocOneOf docs) = Text.intercalate " | " $ map docType docs
docType (DocObject name _) = name

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
