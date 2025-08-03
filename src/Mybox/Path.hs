module Mybox.Path where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List (isPrefixOf, unsnoc)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Records (HasField (..))

data Path = AbsPath {segments :: [Text]} | RelPath {segments :: [Text]}
  deriving (Eq, Ord, Show)

instance ToJSON Path where
  toJSON = toJSON . (.text)
  toEncoding = toEncoding . (.text)

instance FromJSON Path where
  parseJSON = fmap mkPath <$> parseJSON

pRoot :: Path
pRoot = AbsPath []

mkPath :: Text -> Path
mkPath p = case Text.stripPrefix "/" p of
  Just r -> AbsPath $ Text.splitOn "/" r
  Nothing
    | Text.null p -> error "Unexpected empty path"
    | otherwise -> RelPath $ Text.splitOn "/" p

instance HasField "text" Path Text where
  getField (AbsPath s) = "/" <> Text.intercalate "/" s
  getField (RelPath s) = Text.intercalate "/" s

pSegment :: Text -> Path
pSegment s
  | Text.null s = error "Cannot create a path segment from an empty string"
  | Text.isInfixOf "/" s = error $ "Path segments cannot contain slashes: " <> Text.unpack s
  | otherwise = RelPath [s]

instance IsString Path where
  fromString = pSegment . Text.pack

(</>) :: Path -> Path -> Path
_ </> AbsPath b = AbsPath b
AbsPath a </> RelPath b = AbsPath (a <> b)
RelPath a </> RelPath b = RelPath (a <> b)

instance HasField "dirname" Path Path where
  getField (AbsPath s) = case unsnoc s of
    Nothing -> error "Cannot get dirname of an empty path"
    Just (s', _) -> AbsPath s'
  getField (RelPath s) = case unsnoc s of
    Nothing -> error "Cannot get dirname of an empty path"
    Just (s', _) -> RelPath s'

instance HasField "basename" Path Text where
  getField p = case unsnoc p.segments of
    Nothing -> error "Cannot get basename of an empty path"
    Just (_, b) -> b

pUnder :: Path -> Path -> Bool
pUnder (AbsPath a) (AbsPath b) = a `isPrefixOf` b
pUnder (RelPath a) (RelPath b) = a `isPrefixOf` b
pUnder p1 p2 = error $ "Cannot compare paths: " <> show p1 <> " and " <> show p2

pMyboxState :: Path
pMyboxState = RelPath [".local", "share", "mybox"]
