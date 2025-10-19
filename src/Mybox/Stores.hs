module Mybox.Stores (
  Store (..),
  Stores,
  storeGet,
  storeSet,
  storeModify,
  storeModifyM_,
  storeModifyM,
  runStores,
) where

import Data.Dynamic hiding (Dynamic)
import Data.Dynamic qualified as D
import Effectful.Dispatch.Dynamic

import Mybox.LockMap
import Mybox.Prelude

data Store v = Store
  { key :: Text
  , def :: v
  }

data Stores :: Effect where
  GetStore :: Typeable v => Store v -> Stores m (MVar D.Dynamic)

type instance DispatchOf Stores = Dynamic

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
  stores <- newLockMap @_ @Text @D.Dynamic
  interpretWith_ act $
    \case
      GetStore store -> lockMapGet stores store.key $ toDyn store.def
