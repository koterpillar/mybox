module Mybox.Stores (
  Store (..),
  Stores,
  storeVar,
  storeGet,
  storeSet,
  storeModify,
  storeModifyM_,
  storeModifyM,
  runStores,
) where

import Data.Map.Strict qualified as Map
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared

import Mybox.Aeson
import Mybox.Prelude

data Store v = Store
  { key :: Text
  , def :: v
  }
  deriving (Show)

instance FromJSON v => HasField "deserialize" (Store v) (Text -> v) where
  getField _ = either (error . show) id . jsonDecode "store"

type MegaStore = Map Text (MVar Text)

data Stores :: Effect where
  StoreGet :: (FromJSON v, ToJSON v) => Store v -> Stores m (MVar Text)

type instance DispatchOf Stores = Dynamic

storeVar :: (FromJSON v, Stores :> es, ToJSON v) => Store v -> Eff es (MVar Text)
storeVar store = send $ StoreGet store

storeGet :: (Concurrent :> es, FromJSON v, Stores :> es, ToJSON v) => Store v -> Eff es v
storeGet store = do
  v <- storeVar store
  vText <- readMVar v
  pure $ store.deserialize vText

storeSet :: (Concurrent :> es, FromJSON v, Stores :> es, ToJSON v) => Store v -> v -> Eff es ()
storeSet store = storeModify store . const

storeModify :: (Concurrent :> es, FromJSON v, Stores :> es, ToJSON v) => Store v -> (v -> v) -> Eff es ()
storeModify store = storeModifyM_ store . ((.) pure)

storeModifyM_ :: (Concurrent :> es, FromJSON v, Stores :> es, ToJSON v) => Store v -> (v -> Eff es v) -> Eff es ()
storeModifyM_ store f = storeModifyM store $ \v -> (,) () <$> f v

storeModifyM :: (Concurrent :> es, FromJSON v, Stores :> es, ToJSON v) => Store v -> (v -> Eff es (r, v)) -> Eff es r
storeModifyM store f = do
  vv <- storeVar store
  modifyMVar vv $ \vText -> do
    let v = store.deserialize $ vText
    (r, v') <- f v
    let vText' = jsonEncode v'
    pure (vText', r)

runStores :: forall es a. Concurrent :> es => Eff (Stores : es) a -> Eff es a
runStores = reinterpret_
  (evalState $ mempty @MegaStore)
  $ \case
    StoreGet store -> stateM $ \m -> case Map.lookup store.key m of
      Just v -> pure (v, m)
      Nothing -> do
        v <- newMVar $ jsonEncode store.def
        let m' = Map.insert store.key v m
        pure (v, m')
