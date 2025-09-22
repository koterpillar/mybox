module Mybox.Stores (
  Iso,
  textIso,
  jsonIso,
  Stores,
  Store,
  mkStore,
  storeGet,
  storeSet,
  storeDelete,
  storeAdjust,
  storeModify,
  storeClear,
  storeGetAll,
  runStores,
) where

import Data.Map.Strict qualified as Map
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared

import Mybox.Aeson
import Mybox.Prelude

data Iso a b = Iso
  { serialize :: a -> b
  , deserialize :: b -> a
  }

data Store k v = Store
  { storeKey :: Text
  , keyIso :: Iso k Text
  , valueIso :: Iso v Text
  }

textIso :: Iso Text Text
textIso = Iso{serialize = id, deserialize = id}

jsonIso :: (FromJSON v, ToJSON v) => Iso v Text
jsonIso =
  Iso
    { serialize = jsonEncode
    , deserialize = either (error . show) id . jsonDecode "iso"
    }

mkStore :: Text -> Iso k Text -> Iso v Text -> Store k v
mkStore storeKey keyIso valueIso = Store{storeKey, keyIso, valueIso}

data Stores :: Effect where
  StoreGet :: Store k v -> k -> Stores m (Maybe v)
  StoreSet :: Store k v -> k -> v -> Stores m ()
  StoreDelete :: Store k v -> k -> Stores m ()
  StoreClear :: Store k v -> Stores m ()
  StoreGetAll :: Store k v -> Stores m (Map Text v)

type instance DispatchOf Stores = Dynamic

storeGet :: Stores :> es => Store k v -> k -> Eff es (Maybe v)
storeGet store key = send $ StoreGet store key

storeSet :: Stores :> es => Store k v -> k -> v -> Eff es ()
storeSet store key value = send $ StoreSet store key value

storeDelete :: Stores :> es => Store k v -> k -> Eff es ()
storeDelete store key = send $ StoreDelete store key

storeAdjust :: Stores :> es => Store k v -> k -> (Maybe v -> Maybe v) -> Eff es ()
storeAdjust store key f = do
  currentValue <- storeGet store key
  case f currentValue of
    Nothing -> storeDelete store key
    Just newValue -> storeSet store key newValue

storeModify :: Stores :> es => Store k v -> k -> (v -> v) -> Eff es ()
storeModify store key = storeAdjust store key . fmap

storeGetAll :: (Ord k, Stores :> es) => Store k v -> Eff es (Map k v)
storeGetAll store = fmap (Map.mapKeys store.keyIso.deserialize) $ send $ StoreGetAll store

storeClear :: Stores :> es => Store k v -> Eff es ()
storeClear store = send $ StoreClear store

type MegaKey = (Text, Text)

type MegaStore = Map MegaKey Text

runStores :: forall es a. Eff (Stores : es) a -> Eff es a
runStores = reinterpret_
  (evalState $ mempty @MegaStore)
  $ \case
    StoreGet store key ->
      gets $
        fmap store.valueIso.deserialize . Map.lookup (store.storeKey, store.keyIso.serialize key)
    StoreSet store key value ->
      modify $
        Map.insert (store.storeKey, store.keyIso.serialize key) (store.valueIso.serialize value)
    StoreDelete store key ->
      modify $
        Map.delete @_ @Text (store.storeKey, store.keyIso.serialize key)
    StoreClear store -> modify $
      Map.filterWithKey @MegaKey @Text $
        \(k, _) _ -> k /= store.storeKey
    StoreGetAll store ->
      gets $
        fmap store.valueIso.deserialize
          . Map.mapKeys snd
          . Map.filterWithKey @MegaKey @Text (\(k, _) _ -> k == store.storeKey)
