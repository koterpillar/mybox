module StateSpec
  ( spec
  ) where

import           Test.Hspec

import           System.IO.Temp

import           State

data Widget =
  Widget
    { wColor :: String
    , wSize  :: Int
    }
  deriving (Eq, Show)

instance Storable Widget where
  storableFromRow = Widget <$> field <*> field
  storableToRow (Widget color size) = toRow (color, size)
  storableColumns _ = ["color", "size"]

widgetsTable :: Connection -> IO (SQLiteTable Widget)
widgetsTable = sqliteTable "widgets"

mkTempDbFile :: (FilePath -> IO a) -> IO a
mkTempDbFile action =
  withSystemTempFile "StateSpec.sqlite" $ \tempFile _ -> action tempFile

spec :: Spec
spec = do
  describe "sqliteDB" $ do
    around mkTempDbFile $ do
      it "retrieves a stored value" $ \file -> do
        withSqliteDB file $ \db -> do
          table <- widgetsTable db
          storePut "widget1" (Widget "red" 10) table
        withSqliteDB file $ \db -> do
          table <- widgetsTable db
          widget <- storeGet "widget1" table
          widget `shouldBe` Just (Widget "red" 10)
