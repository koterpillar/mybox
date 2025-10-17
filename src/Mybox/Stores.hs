module Mybox.Stores (
  Store (..),
  StoreData,
  emptyStoreData,
  Stores,
  storeGet,
  storeSet,
  storeModify,
  storeModifyM_,
  storeModifyM,
  storeLock,
  storeReset,
  runStores,
  runStoresWith,
) where

import Data.Dynamic hiding (Dynamic)
import Data.Dynamic qualified
import Data.Map.Strict qualified as Map
import Effectful.Dispatch.Dynamic

import Mybox.Prelude

data Store v = Store
  { key :: Text
  , def :: v
  }

type DDynamic = Data.Dynamic.Dynamic

data StoreData = StoreData
  { locks :: Map Text (MVar ())
  , stores :: Map Text (MVar DDynamic)
  }

emptyStoreData :: StoreData
emptyStoreData = StoreData{locks = Map.empty, stores = Map.empty}

data Stores :: Effect where
  GetLock :: Text -> Stores m (MVar ())
  GetStore :: Typeable v => Store v -> Stores m (MVar DDynamic)
  Reset :: Stores m ()

type instance DispatchOf Stores = Dynamic

storeLock :: (Concurrent :> es, Stores :> es) => Text -> Eff es r -> Eff es r
storeLock key act = do
  v <- send $ GetLock key
  atomicMVar v act

storeGet :: (Concurrent :> es, Stores :> es, Typeable v) => Store v -> Eff es v
storeGet store = do
  v <- send $ GetStore store
  dv <- readMVar v
  pure $ fromDyn dv store.def

storeSet :: (Concurrent :> es, Stores :> es, Typeable v) => Store v -> v -> Eff es ()
storeSet store = storeModify store . const

storeModify :: (Concurrent :> es, Stores :> es, Typeable v) => Store v -> (v -> v) -> Eff es ()
storeModify store = storeModifyM_ store . ((.) pure)

storeModifyM_ :: (Concurrent :> es, Stores :> es, Typeable v) => Store v -> (v -> Eff es v) -> Eff es ()
storeModifyM_ store f = storeModifyM store $ \v -> (,) () <$> f v

storeModifyM :: (Concurrent :> es, Stores :> es, Typeable v) => Store v -> (v -> Eff es (r, v)) -> Eff es r
storeModifyM store f = do
  vv <- send $ GetStore store
  modifyMVar vv $ \dv -> do
    let v = fromDyn dv store.def
    (r, v') <- f v
    let dv' = toDyn v'
    pure (dv', r)

storeReset :: Stores :> es => Eff es ()
storeReset = send Reset

runStores :: Concurrent :> es => Eff (Stores : es) a -> Eff es a
runStores act = do
  s <- newMVar emptyStoreData
  runStoresWith s act

runStoresWith :: Concurrent :> es => MVar StoreData -> Eff (Stores : es) a -> Eff es a
runStoresWith s = interpret_ $
  \case
    GetLock key -> modifyMVar s $ \m -> case Map.lookup key m.locks of
      Just v -> pure (m, v)
      Nothing -> do
        v <- newMVar ()
        let m' = m{locks = Map.insert key v m.locks}
        pure (m', v)
    GetStore store -> modifyMVar s $ \m -> case Map.lookup store.key m.stores of
      Just v -> pure (m, v)
      Nothing -> do
        v <- newMVar $ toDyn store.def
        let m' = m{stores = Map.insert store.key v m.stores}
        pure (m', v)
    Reset -> modifyMVarPure s $ \m -> m{stores = Map.empty}
