module State
  ( field
  , toRow
  , Connection
  , Storable(..)
  , SQLiteTable
  , storeGet
  , storePut
  , storeDelete
  , storeFind
  , storeAppend
  , storeFind_
  , withSqliteDB
  , withSqliteMemoryDB
  , sqliteTable
  ) where

import           Data.String                    (fromString)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

import           UnliftIO

class Storable item where
  storableFromRow :: RowParser item
  storableToRow :: item -> [SQLData]
  storableColumns :: proxy item -> [Text]

type Key = Text

withSqliteDB :: MonadUnliftIO m => FilePath -> (Connection -> m a) -> m a
withSqliteDB path action =
  withRunInIO $ \runInIO -> withConnection path $ runInIO . action

withSqliteMemoryDB :: MonadUnliftIO m => (Connection -> m a) -> m a
withSqliteMemoryDB = withSqliteDB ":memory:"

data SQLiteTable item =
  SQLiteTable
    { slsTable      :: Text
    , slsConnection :: Connection
    }

fromText :: Text -> Query
fromText = fromString . Text.unpack

storableColumnCount :: Storable item => proxy item -> Int
storableColumnCount = length . storableColumns

sqliteTable ::
     forall item m. (Storable item, MonadIO m)
  => Text
  -> Connection
  -> m (SQLiteTable item)
sqliteTable name conn = do
  let attributes = Text.intercalate ", " $ storableColumns (Nothing @item)
  let createStmt =
        fromText $
        "CREATE TABLE IF NOT EXISTS " <>
        name <> " (id TEXT PRIMARY KEY, " <> attributes <> ")"
  liftIO $ execute_ conn createStmt
  pure $ SQLiteTable name conn

storeGet ::
     (MonadIO m, Storable item) => Key -> SQLiteTable item -> m (Maybe item)
storeGet key table = storeFind [("id", key)] table >>= \case
  [] -> pure Nothing
  [(_, item)] -> pure $ Just item
  _ -> error "storeGet: multiple items with the same key"

data WithKey item =
  WithKey Key item

instance Storable item => FromRow (WithKey item) where
  fromRow = WithKey <$> field <*> storableFromRow

instance Storable item => ToRow (WithKey item) where
  toRow (WithKey key item) = SQLText key : storableToRow item

storePut ::
     (MonadIO m, Storable item) => Key -> item -> SQLiteTable item -> m ()
storePut key item (SQLiteTable name conn) = do
  let attributes =
        Text.intercalate ", " $ replicate (storableColumnCount (Just item)) "?"
  let insertStmt =
        fromText $
        "INSERT OR REPLACE INTO " <> name <> " VALUES (?, " <> attributes <> ")"
  liftIO $ execute conn insertStmt (WithKey key item)

storeDelete :: MonadIO m => Key -> SQLiteTable item -> m ()
storeDelete = error "storeDelete not implemented"

storeFind :: (MonadIO m, Storable item) => [(Text, Text)] -> SQLiteTable item -> m [(Key, item)]
storeFind conditions (SQLiteTable name conn) = do
  let whereClause = Text.intercalate " AND " [k <> " = ?" | (k, _) <- conditions]
  let selectStmt = fromText $ "SELECT * FROM " <> name <> " WHERE " <> whereClause
  results <- liftIO $ query conn selectStmt (fmap snd conditions)
  pure [(key, item) | WithKey key item <- results]

storeAppend :: MonadIO m => item -> SQLiteTable item -> m Text
storeAppend = error "storeAppend not implemented"

storeFind_ :: (MonadIO m, Storable item) => [(Text, Text)] -> SQLiteTable item -> m [item]
storeFind_ = fmap (fmap (fmap (map snd))) storeFind
