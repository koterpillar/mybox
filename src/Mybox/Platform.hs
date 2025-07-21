module Mybox.Platform (
  Architecture (..),
  architectureFilters,
  OS (..),
  Distribution (..),
  osFilters,
) where

import Data.Map.Strict qualified as Map

import Mybox.Filters
import Mybox.Prelude

data Architecture = X86_64 | Aarch64 deriving (Eq, Ord, Show)

archSynonyms :: Map Text [Text]
archSynonyms =
  Map.fromList
    [ ("aarch64", ["aarch64", "arm64", "arm"])
    , ("i386", ["i386", "i686", "x86"])
    , ("mips", [])
    , ("powerpc", ["ppc"])
    , ("s390x", [])
    , ("x86_64", ["x86_64", "amd64", "x64"])
    ]

architectureFilters :: Architecture -> [Text -> Bool]
architectureFilters = fromSynonyms archSynonyms . akey
 where
  akey X86_64 = "x86_64"
  akey Aarch64 = "aarch64"

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
