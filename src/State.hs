module State
  ( field
  , toRow
  , Connection
  , Only(..)
  , Storable(..)
  , SQLiteTable
  , storeGet
  , storePut
  , storeDel
  , storeDelete
  , storeFind
  , storeFind_
  , storeAppend
  , withSqliteDB
  , withSqliteMemoryDB
  , sqliteTable
  ) where

import           Data.String                    (fromString)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text

import qualified Data.UUID                      as UUID
import qualified Data.UUID.V4                   as UUID

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
  SQLiteTable Text Connection

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
storeGet key table =
  storeFind [("id", key)] table >>= \case
    []          -> pure Nothing
    [(_, item)] -> pure $ Just item
    _           -> error "storeGet: multiple items with the same key"

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

storeDel :: MonadIO m => Key -> SQLiteTable item -> m ()
storeDel key = storeDelete [("id", key)]

storeDelete :: MonadIO m => [(Text, Text)] -> SQLiteTable item -> m ()
storeDelete conditions (SQLiteTable name conn) = do
  let (wherePart, values) = mkWhere conditions
  let selectStmt = fromText $ "DELETE FROM " <> name <> wherePart
  liftIO $ execute conn selectStmt values

storeFind ::
     (MonadIO m, Storable item)
  => [(Text, Text)]
  -> SQLiteTable item
  -> m [(Key, item)]
storeFind conditions (SQLiteTable name conn) = do
  let (wherePart, values) = mkWhere conditions
  let selectStmt = fromText $ "SELECT * FROM " <> name <> wherePart
  results <- liftIO $ query conn selectStmt values
  pure [(key, item) | WithKey key item <- results]

mkWhere :: [(Text, Text)] -> (Text, [Text])
mkWhere conditions = (wherePart, fmap snd conditions)
  where
    whereClause = Text.intercalate " AND " [k <> " = ?" | (k, _) <- conditions]
    wherePart =
      if Text.null whereClause
        then ""
        else " WHERE " <> whereClause

storeAppend :: (MonadIO m, Storable item) => item -> SQLiteTable item -> m Text
storeAppend item table = do
  key <- UUID.toText <$> liftIO UUID.nextRandom
  storePut key item table
  pure key

storeFind_ ::
     (MonadIO m, Storable item)
  => [(Text, Text)]
  -> SQLiteTable item
  -> m [item]
storeFind_ = fmap (fmap (fmap (map snd))) storeFind
