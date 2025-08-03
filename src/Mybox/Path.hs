module Mybox.Path (
  AnchorC,
  Anchor,
  Abs,
  Rel,
  Path,
  (</>),
  mkPath,
  pAbs,
  pRoot,
  pSegment,
  pRelativeTo,
  pRelativeTo_,
  pUnder,
  pMyboxState,
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Function ((&))
import Data.List (isPrefixOf, unsnoc)
import Data.Maybe (isJust)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Records (HasField (..))

data Abs = Abs_ deriving (Eq, Ord, Show)

data Rel = Rel_ deriving (Eq, Ord, Show)

data Anchor = Abs | Rel deriving (Eq, Ord, Show)

class (Eq a, Ord a) => AnchorC a where
  toAnchor :: a -> Anchor
  mkPath :: Text -> Path a

instance AnchorC Abs where
  toAnchor _ = Abs
  mkPath t =
    let p = mkPath t
     in case p.anchor_ of
          Abs -> Path Abs_ p.segments
          Rel -> error $ "Path is not absolute: " <> Text.unpack t

instance AnchorC Rel where
  toAnchor _ = Rel
  mkPath t =
    let p = mkPath t
     in case p.anchor_ of
          Rel -> Path Rel_ p.segments
          Abs -> error $ "Path is not relative: " <> Text.unpack t

instance AnchorC Anchor where
  toAnchor = id
  mkPath t = case Text.stripPrefix "/" t of
    Just r -> Path Abs $ Text.splitOn "/" r
    Nothing -> Path Rel $ Text.splitOn "/" t

data Path a = Path {anchor_ :: a, segments :: [Text]}
  deriving (Eq, Ord, Show)

instance AnchorC a => HasField "anchor" (Path a) Anchor where
  getField = toAnchor . (.anchor_)

instance AnchorC a => HasField "text" (Path a) Text where
  getField p = Text.intercalate "/" p.segments & (case p.anchor of Abs -> ("/" <>); Rel -> id)

instance AnchorC a => ToJSON (Path a) where
  toJSON = toJSON . (.text)
  toEncoding = toEncoding . (.text)

instance FromJSON (Path Anchor) where
  parseJSON = fmap mkPath <$> parseJSON

pRoot :: Path Abs
pRoot = Path Abs_ []

pAbs :: AnchorC a => Path a -> Maybe (Path Abs)
pAbs p
  | p.anchor == Abs = Just $ Path Abs_ p.segments
  | otherwise = Nothing

pSegment :: Text -> Path Rel
pSegment s
  | Text.null s = error "Cannot create a path segment from an empty string"
  | Text.isInfixOf "/" s = error $ "Path segments cannot contain slashes: " <> Text.unpack s
  | otherwise = Path Rel_ [s]

instance IsString (Path Rel) where
  fromString = pSegment . Text.pack

class AnchorAppend a1 a2 a3 | a1 a2 -> a3 where
  anchorAppend :: a1 -> a2 -> a3

instance AnchorC a => AnchorAppend Abs a Abs where
  anchorAppend _ _ = Abs_

instance AnchorC a => AnchorAppend Rel a a where
  anchorAppend _ a = a

instance AnchorAppend Anchor Abs Abs where
  anchorAppend _ _ = Abs_

instance AnchorAppend Anchor Rel Anchor where
  anchorAppend a _ = a

instance AnchorAppend Anchor Anchor Anchor where
  anchorAppend Abs _ = Abs
  anchorAppend _ Abs = Abs
  anchorAppend Rel Rel = Rel

(</>) :: (AnchorAppend a1 a2 a3, AnchorC a2) => Path a1 -> Path a2 -> Path a3
p1 </> p2 =
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
  | a `isPrefixOf` b = Just $ Path Rel_ $ drop (length b) a
  | otherwise = Nothing

pRelativeTo_ :: Path Abs -> Path Abs -> Path Rel
pRelativeTo_ a b =
  case pRelativeTo a b of
    Just rel -> rel
    Nothing -> error $ "Cannot get relative path from " <> show a <> " to " <> show b

pUnder :: Path Abs -> Path Abs -> Bool
pUnder a b = isJust $ pRelativeTo a b

pMyboxState :: Path Rel
pMyboxState = Path Rel_ [".local", "share", "mybox"]
