module Mybox.Stores (
  Iso,
  textIso,
  jsonIso,
  Store (..),
  Stores,
  storeVar,
  storeLocked,
  storeInitLocked,
  storeGet,
  storeSet,
  storeModify,
  storeModifyM_,
  storeModifyM,
  runStores,
  MegaStore,
  runStoresWith,
) where

import Data.Map.Strict qualified as Map
import Effectful.Dispatch.Dynamic

import Mybox.Aeson
import Mybox.Prelude

data Iso a b = Iso
  { serialize :: a -> b
  , deserialize :: b -> a
  }

data Store v = Store
  { key :: Text
  , iso :: Iso v Text
  , def :: v
  }

textIso :: Iso Text Text
textIso = Iso{serialize = id, deserialize = id}

jsonIso :: (FromJSON v, ToJSON v) => Iso v Text
jsonIso =
  Iso
    { serialize = jsonEncode
    , deserialize = either (error . show) id . jsonDecode "iso"
    }

type MegaStore = Map Text (MVar Text)

data Stores :: Effect where
  StoreGet :: Store v -> Stores m (MVar Text)

type instance DispatchOf Stores = Dynamic

storeVar :: Stores :> es => Store v -> Eff es (MVar Text)
storeVar store = send $ StoreGet store

storeLocked :: (Concurrent :> es, Stores :> es) => Store () -> Eff es a -> Eff es a
storeLocked s act = storeModifyM s $ \() -> (,) <$> act <*> pure ()

storeInitLocked :: (Concurrent :> es, Stores :> es) => Store (Maybe a) -> Eff es a -> Eff es a
storeInitLocked s act = storeModifyM s $ \case
  Just r -> pure (r, Just r)
  Nothing -> do
    r <- act
    pure (r, Just r)

storeGet :: (Concurrent :> es, Stores :> es) => Store v -> Eff es v
storeGet store = do
  v <- storeVar store
  vText <- readMVar v
  pure $ store.iso.deserialize vText

storeSet :: (Concurrent :> es, Stores :> es) => Store v -> v -> Eff es ()
storeSet store = storeModify store . const

storeModify :: (Concurrent :> es, Stores :> es) => Store v -> (v -> v) -> Eff es ()
storeModify store = storeModifyM_ store . ((.) pure)

storeModifyM_ :: (Concurrent :> es, Stores :> es) => Store v -> (v -> Eff es v) -> Eff es ()
storeModifyM_ store f = storeModifyM store $ \v -> (,) () <$> f v

storeModifyM :: (Concurrent :> es, Stores :> es) => Store v -> (v -> Eff es (r, v)) -> Eff es r
storeModifyM store f = do
  vv <- storeVar store
  modifyMVar vv $ \vText -> do
    let v = store.iso.deserialize vText
    (r, v') <- f v
    let vText' = store.iso.serialize v'
    pure (vText', r)

runStores :: forall es a. Concurrent :> es => Eff (Stores : es) a -> Eff es a
runStores act = do
  stores <- newMVar Map.empty
  runStoresWith stores act

runStoresWith :: forall es a. Concurrent :> es => MVar MegaStore -> Eff (Stores : es) a -> Eff es a
runStoresWith stores act = do
  interpretWith_ act $
    \case
      StoreGet store -> modifyMVar stores $ \m -> case Map.lookup store.key m of
        Just v -> pure (m, v)
        Nothing -> do
          v <- newMVar $ store.iso.serialize store.def
          let m' = Map.insert store.key v m
          pure (m', v)
