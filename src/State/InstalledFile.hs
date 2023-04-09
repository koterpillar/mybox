module State.InstalledFile
  ( InstalledFile(..)
  , installedFiles
  ) where

import           Data.Text (Text)

import           State

data InstalledFile =
  InstalledFile
    { ifPackage :: Text
    , ifPath    :: Text
    , ifRoot    :: Bool
    }
  deriving (Eq, Show)

instance Storable InstalledFile where
  storableFromRow = InstalledFile <$> field <*> field <*> field
  storableToRow (InstalledFile package path root) = toRow (package, path, root)
  storableColumns _ = ["package", "path", "root"]

installedFiles :: Connection -> IO (SQLiteTable InstalledFile)
installedFiles = sqliteTable "installed_file"
