module Mybox.Release (
  Release (..),
  mkRelease,
  defaultVersion,
  defaultRelease,
  mkSingleRelease,
) where

import Mybox.Prelude

data Release = Release
  { version :: Text
  , date :: Maybe UTCTime
  , prerelease :: Bool
  }
  deriving (Eq, Show)

mkRelease :: Text -> Release
mkRelease version =
  Release
    { version
    , date = Nothing
    , prerelease = False
    }

defaultVersion :: Text
defaultVersion = "latest"

defaultRelease :: [Release]
defaultRelease = mkSingleRelease defaultVersion

mkSingleRelease :: Text -> [Release]
mkSingleRelease = pure . mkRelease
