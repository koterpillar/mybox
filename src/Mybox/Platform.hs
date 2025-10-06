module Mybox.Platform (
  Architecture (..),
  architectureString,
  parseArchitecture,
  architectureFilters,
  OS (..),
  Distribution (..),
  osFilters,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Filters
import Mybox.Prelude

findKey :: (Eq a, Foldable t) => Map b (t a) -> a -> Maybe b
findKey m s = fst <$> find (elem s . snd) (Map.toList m)

data Architecture = X86_64 | Aarch64 deriving (Eq, Ord, Show)

archStrings :: Map Architecture (NonEmpty Text)
archStrings =
  Map.fromList
    [ (X86_64, "x86_64" :| ["amd64", "x64"])
    , (Aarch64, "aarch64" :| ["arm64", "arm"])
    ]

architectureString :: Architecture -> Text
architectureString = NonEmpty.head . fromJust . flip Map.lookup archStrings

parseArchitecture :: Text -> Either String Architecture
parseArchitecture a =
  maybe (Left $ "Unsupported architecture: " <> Text.unpack a) pure $ findKey archStrings a

allArchSynonyms :: Map Text [Text]
allArchSynonyms =
  Map.fromList $
    map (\(a :| as) -> (a, a : as)) (toList archStrings)
      <> [ ("i386", ["i386", "i686", "x86"])
         , ("mips", [])
         , ("powerpc", ["ppc"])
         , ("s390x", [])
         ]

instance ToJSON Architecture where
  toJSON = String . architectureString

instance FromJSON Architecture where
  parseJSON = withText "Architecture" $ \s -> either fail pure $ parseArchitecture s

architectureFilters :: Architecture -> [Text -> Bool]
architectureFilters = fromSynonyms allArchSynonyms . architectureString

data Distribution = Debian {variant :: Text} | Fedora deriving (Eq, Ord, Show)

data OS = Linux {distribution :: Distribution} | MacOS deriving (Eq, Ord, Show)

osSynonyms :: Map Text [Text]
osSynonyms =
  Map.fromList
    [ ("darwin", ["darwin", "macos", "osx"])
    , ("linux", ["linux"])
    , ("windows", ["windows"])
    ]

osFilters :: OS -> [Text -> Bool]
osFilters = fromSynonyms osSynonyms . okey
 where
  okey MacOS = "darwin"
  okey (Linux _) = "linux"
