module Mybox.Stores (
  Store (..),
  Stores,
  storeGet,
  storeSet,
  storeModify,
  storeModifyM_,
  storeModifyM,
  storeLock,
  runStores,
) where

import Data.Dynamic hiding (Dynamic)
import Data.Dynamic qualified
import Effectful.Dispatch.Dynamic

import Mybox.LockMap
import Mybox.Prelude

data Store v = Store
  { key :: Text
  , def :: v
  }

type DDynamic = Data.Dynamic.Dynamic

data Stores :: Effect where
  GetLock :: Text -> Stores m (MVar ())
  GetStore :: Typeable v => Store v -> Stores m (MVar DDynamic)

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

runStores :: forall es a. Concurrent :> es => Eff (Stores : es) a -> Eff es a
runStores act = do
  locks <- newLockMap @_ @Text @()
  stores <- newLockMap @_ @Text @DDynamic
  interpretWith_ act $
    \case
      GetLock key -> lockMapGet locks key ()
      GetStore store -> lockMapGet stores store.key $ toDyn store.def
