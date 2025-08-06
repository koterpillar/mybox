module Mybox.Path (
  Anchor (..),
  AnyAnchor (..),
  Abs,
  Rel,
  Path,
  segments,
  (</>),
  (<//>),
  mkPath,
  pAbs,
  pWiden,
  pSegment,
  pRelativeTo,
  pRelativeTo_,
  pUnder,
  pRoot,
  pMyboxState,
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List (isPrefixOf, unsnoc)
import Data.Maybe (isJust)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)

data Abs = Abs_ deriving (Eq, Ord, Show)

data Rel = Rel_ deriving (Eq, Ord, Show)

data AnyAnchor = Abs | Rel deriving (Eq, Ord, Show)

class (Eq a, Ord a) => Anchor a where
  toAnchor :: a -> AnyAnchor
  mkPath_ :: Text -> Either String (Path a)

instance Anchor Abs where
  toAnchor _ = Abs
  mkPath_ t = do
    p <- mkPath_ t
    case p.anchor_ of
      Abs -> Right $ Path Abs_ p.segments
      Rel -> Left $ "Path is not absolute: " <> Text.unpack t

instance Anchor Rel where
  toAnchor _ = Rel
  mkPath_ t = do
    p <- mkPath_ t
    case p.anchor_ of
      Rel -> Right $ Path Rel_ p.segments
      Abs -> Left $ "Path is not relative: " <> Text.unpack t

instance Anchor AnyAnchor where
  toAnchor = id
  mkPath_ t = Right $ case Text.stripPrefix "/" t of
    Just r -> Path Abs $ split r
    Nothing -> Path Rel $ split t
   where
    split = filter (\s -> not (Text.null s) && s /= ".") . Text.splitOn "/"

mkPath :: (Anchor a, HasCallStack) => Text -> Path a
mkPath = either error id . mkPath_

data Path a = Path {anchor_ :: a, segments :: [Text]}
  deriving (Eq, Ord)

instance (Anchor a, Show a) => Show (Path a) where
  show p = "mkPath " <> show p.text

instance Anchor a => HasField "anchor" (Path a) AnyAnchor where
  getField = toAnchor . (.anchor_)

instance Anchor a => HasField "text" (Path a) Text where
  getField p = case (p.anchor, p.segments) of
    (Abs, s) -> "/" <> Text.intercalate "/" s
    (Rel, []) -> "."
    (Rel, s) -> Text.intercalate "/" s

instance Anchor a => ToJSON (Path a) where
  toJSON = toJSON . (.text)
  toEncoding = toEncoding . (.text)

instance Anchor a => FromJSON (Path a) where
  parseJSON v = do
    t <- parseJSON v
    either fail pure $ mkPath_ t

pRoot :: Path Abs
pRoot = Path Abs_ []

pAbs :: Anchor a => Path a -> Maybe (Path Abs)
pAbs p
  | p.anchor == Abs = Just $ Path Abs_ p.segments
  | otherwise = Nothing

pWiden :: Anchor a => Path a -> Path AnyAnchor
pWiden (Path a s) = Path (toAnchor a) s

pSegment :: HasCallStack => Text -> Path Rel
pSegment s
  | Text.null s = error "Cannot create a path segment from an empty string"
  | Text.isInfixOf "/" s = error $ "Path segments cannot contain slashes: " <> Text.unpack s
  | otherwise = Path Rel_ [s]

instance IsString (Path Rel) where
  fromString = pSegment . Text.pack

class AnchorAppend a1 a2 a3 | a1 a2 -> a3 where
  anchorAppend :: a1 -> a2 -> a3

instance Anchor a => AnchorAppend Abs a Abs where
  anchorAppend _ _ = Abs_

instance Anchor a => AnchorAppend Rel a a where
  anchorAppend _ a = a

instance AnchorAppend AnyAnchor Abs Abs where
  anchorAppend _ _ = Abs_

instance AnchorAppend AnyAnchor Rel AnyAnchor where
  anchorAppend a _ = a

instance AnchorAppend AnyAnchor AnyAnchor AnyAnchor where
  anchorAppend Abs _ = Abs
  anchorAppend _ Abs = Abs
  anchorAppend Rel Rel = Rel

(</>) :: HasCallStack => AnchorAppend a Rel a => Path a -> Text -> Path a
p </> s = p <//> pSegment s

(<//>) :: (Anchor a2, AnchorAppend a1 a2 a3) => Path a1 -> Path a2 -> Path a3
p1 <//> p2 =
  Path (anchorAppend p1.anchor_ p2.anchor_) $
    case p2.anchor of
      Abs -> p2.segments
      Rel -> p1.segments <> p2.segments

instance HasField "dirname" (Path a) (Path a) where
  getField (Path a s) = case unsnoc s of
    Nothing -> error "Cannot get dirname of an empty path"
    Just (s', _) -> Path a s'

instance HasField "basename" (Path a) Text where
  getField p = case unsnoc p.segments of
    Nothing -> error "Cannot get basename of an empty path"
    Just (_, b) -> b

pRelativeTo :: Path Abs -> Path Abs -> Maybe (Path Rel)
pRelativeTo (Path Abs_ a) (Path Abs_ b)
  | a `isPrefixOf` b = Just $ Path Rel_ $ drop (length a) b
  | otherwise = Nothing

pRelativeTo_ :: HasCallStack => Path Abs -> Path Abs -> Path Rel
pRelativeTo_ a b =
  case pRelativeTo a b of
    Just rel -> rel
    Nothing -> error $ "Cannot get relative path from " <> show a <> " to " <> show b

pUnder :: Path Abs -> Path Abs -> Bool
pUnder a b = isJust $ pRelativeTo a b

pMyboxState :: Path Rel
pMyboxState = pSegment ".local" </> "share" </> "mybox"
