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

import Data.Dynamic hiding (Dynamic)
import Data.Dynamic qualified
import Data.Map.Strict qualified as Map
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared

import Mybox.Prelude

data Store v = Store
  { key :: Text
  , def :: v
  }

type DDynamic = Data.Dynamic.Dynamic

type MegaStore = Map Text (MVar DDynamic)

data Stores :: Effect where
  StoreGet :: Typeable v => Store v -> Stores m (MVar DDynamic)

type instance DispatchOf Stores = Dynamic

storeVar :: (Stores :> es, Typeable v) => Store v -> Eff es (MVar DDynamic)
storeVar store = send $ StoreGet store

storeGet :: (Concurrent :> es, Stores :> es, Typeable v) => Store v -> Eff es v
storeGet store = do
  v <- storeVar store
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
  vv <- storeVar store
  modifyMVar vv $ \dv -> do
    let v = fromDyn dv store.def
    (r, v') <- f v
    let dv' = toDyn v'
    pure (dv', r)

runStores :: forall es a. Concurrent :> es => Eff (Stores : es) a -> Eff es a
runStores = reinterpret_
  (evalState $ mempty @MegaStore)
  $ \case
    StoreGet store -> stateM $ \m -> case Map.lookup store.key m of
      Just v -> pure (v, m)
      Nothing -> do
        v <- newMVar $ toDyn store.def
        let m' = Map.insert store.key v m
        pure (v, m')
