{-# LANGUAGE TypeFamilies #-}

module Mybox.Stores (Stores, Store, storeGet, storeSet, storeDelete, runStores, textStore, jsonStore) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local

import Mybox.Aeson
import Mybox.Prelude

data Store v = Store {key :: Text, serialize :: v -> Text, deserialize :: Text -> v}

textStore :: Text -> Store Text
textStore k = Store{key = k, serialize = id, deserialize = id}

jsonStore :: (FromJSON v, ToJSON v) => Text -> Store v
jsonStore k =
  Store
    { key = k
    , serialize = jsonEncode
    , deserialize = either (error . show) id . jsonDecode (show k)
    }

data Stores :: Effect where
  StoreGet :: Store v -> Text -> Stores m (Maybe v)
  StoreSet :: Store v -> Text -> v -> Stores m ()
  StoreDelete :: Store v -> Text -> Stores m ()

type instance DispatchOf Stores = Dynamic

storeGet :: Stores :> es => Store v -> Text -> Eff es (Maybe v)
storeGet store key = send $ StoreGet store key

storeSet :: Stores :> es => Store v -> Text -> v -> Eff es ()
storeSet store key value = send $ StoreSet store key value

storeDelete :: Stores :> es => Store v -> Text -> Eff es ()
storeDelete store key = send $ StoreDelete store key

type MegaStore = Map (Text, Text) Text

runStores :: forall es a. Eff (Stores : es) a -> Eff es a
runStores = reinterpret_
  (evalState $ mempty @MegaStore)
  $ \case
    StoreGet store key -> gets $ fmap store.deserialize . Map.lookup (store.key, key)
    StoreSet store key value -> modify $ Map.insert (store.key, key) (store.serialize value)
    StoreDelete store key -> modify $ Map.delete @_ @Text (store.key, key)
