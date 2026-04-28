module Mybox.LockMap (
  LockMap,
  newLockMap,
  lockMapGet,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Debug.Trace
import Effectful.Concurrent.MVar
import System.Random

import Mybox.Prelude

-- | A concurrent map that provides atomic access to inner MVars by key.
data LockMap k v = LockMap
  { caller :: Text
  , mvar :: MVar (Map k (MVar v))
  }

shouldTraceCaller :: Text -> Bool
shouldTraceCaller src =
  not (Text.isPrefixOf "Stores" src || Text.isPrefixOf "LockMapSpec" src)

-- | Create a new empty LockMap.
newLockMap :: (Concurrent :> es, IOE :> es) => Text -> Eff es (LockMap k v)
newLockMap caller = do
  suffix <- liftIO (randomIO @Word)
  let caller' = caller <> "-" <> Text.pack (show suffix)
  when (shouldTraceCaller caller') $ traceM $ "LMAP: create " <> Text.unpack caller'
  LockMap caller' <$> newMVar Map.empty

-- | Atomically get or create an MVar for the given key.
-- If the key already exists, returns the existing MVar.
-- If the key doesn't exist, creates a new MVar with the default value and returns it.
lockMapGet :: (Concurrent :> es, Ord k, Show k) => LockMap k v -> k -> v -> Eff es (MVar v)
lockMapGet lockMap key defaultValue = do
  when (shouldTraceCaller lockMap.caller) $ traceM $ "LMAP get " <> Text.unpack lockMap.caller <> " " <> show key

  let mmVar = lockMap.mvar
  modifyMVar mmVar $ \mm ->
    case Map.lookup key mm of
      Just v -> pure (mm, v)
      Nothing -> do
        v <- newMVar defaultValue
        let mm' = Map.insert key v mm
        pure (mm', v)
