module State.Version
  ( Version(..)
  , versions
  ) where

import           Data.Text (Text)

import           State

data Version =
  Version
    { vVersion :: Text
    }
  deriving (Eq, Show)

instance Storable Version where
  storableFromRow = Version <$> field
  storableToRow (Version version) = toRow (Only version)
  storableColumns _ = ["version"]

versions :: Connection -> IO (SQLiteTable Version)
versions = sqliteTable "version"
