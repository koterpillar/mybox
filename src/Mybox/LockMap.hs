module Mybox.LockMap (
  LockMap,
  newLockMap,
  lockMapGet,
) where

import Data.Map.Strict qualified as Map
import Effectful.Concurrent.MVar

import Mybox.Prelude

-- | A concurrent map that provides atomic access to inner MVars by key.
newtype LockMap k v = LockMap (MVar (Map k (MVar v)))

-- | Create a new empty LockMap.
newLockMap :: Concurrent :> es => Eff es (LockMap k v)
newLockMap = LockMap <$> newMVar Map.empty

-- | Atomically get or create an MVar for the given key.
-- If the key already exists, returns the existing MVar.
-- If the key doesn't exist, creates a new MVar with the default value and returns it.
lockMapGet :: (Concurrent :> es, Ord k) => LockMap k v -> k -> v -> Eff es (MVar v)
lockMapGet (LockMap mmVar) key defaultValue = do
  modifyMVar mmVar $ \mm ->
    case Map.lookup key mm of
      Just v -> pure (mm, v)
      Nothing -> do
        v <- newMVar defaultValue
        let mm' = Map.insert key v mm
        pure (mm', v)
