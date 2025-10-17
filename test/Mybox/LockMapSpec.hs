module Mybox.LockMapSpec where

import Effectful.Concurrent (threadDelay)

import Mybox.LockMap
import Mybox.Prelude
import Mybox.Spec.Utils
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "LockMap" $ do
    it "returns the same MVar for the same key" $ do
      lockMap <- newLockMap @_ @Text @Int
      mvar1 <- lockMapGet lockMap "test" 42
      mvar2 <- lockMapGet lockMap "test" 99
      -- The MVars should be identical (same reference)
      (mvar1 == mvar2) `shouldBe` True
      -- And the value should be the original default, not the second one
      readMVar mvar1 >>= (`shouldBe` 42)

    it "returns different MVars for different keys" $ do
      lockMap <- newLockMap @_ @Text @Int
      mvar1 <- lockMapGet lockMap "key1" 10
      mvar2 <- lockMapGet lockMap "key2" 20
      -- The MVars should be different
      (mvar1 /= mvar2) `shouldBe` True
      -- Each should have its own default value
      readMVar mvar1 >>= (`shouldBe` 10)
      readMVar mvar2 >>= (`shouldBe` 20)

    it "is thread-safe when creating MVars concurrently" $ do
      lockMap <- newLockMap @_ @Int @Text
      mvarsVar <- newMVar ([] :: [MVar Text])

      -- Launch 100 concurrent operations trying to get the same key
      concurrently_ 100 $ do
        mvar <- lockMapGet lockMap 1 "default"
        modifyMVarPure mvarsVar (mvar :)

      -- All MVars should be identical
      mvars <- readMVar mvarsVar
      length mvars `shouldBe` 100
      let uniqueMVars = length $ foldl (\acc mv -> if mv `elem` acc then acc else mv : acc) [] mvars
      uniqueMVars `shouldBe` 1

    it "correctly handles modifications to different keys concurrently" $ do
      lockMap <- newLockMap @_ @Int @Int

      -- Create MVars for different keys and modify them concurrently
      concurrently 100 $ \keyNum -> do
        mvar <- lockMapGet lockMap keyNum (0 :: Int)
        -- Increment the value 10 times
        replicateM_ 10 $ modifyMVar_ mvar (pure . succ)

      -- Check that each key has the correct final value
      forM_ [1 .. 10] $ \keyNum -> do
        mvar <- lockMapGet lockMap keyNum 999 -- Should get existing MVar, not create new one
        readMVar mvar >>= (`shouldBe` 10)

    it "maintains consistency under heavy concurrent access to same key" $ do
      lockMap <- newLockMap @_ @Text @Int

      -- 50 threads all incrementing the same counter
      concurrently_ 100 $ do
        mvar <- lockMapGet lockMap "counter" 0
        -- Each thread increments 20 times
        replicateM_ 20 $ do
          modifyMVar_ mvar (pure . succ)
          threadDelay 1 -- Small delay to increase chance of race conditions

      -- The final value should be 100 * 20 = 2000
      mvar <- lockMapGet lockMap "counter" 999
      readMVar mvar >>= (`shouldBe` 2000)
