module Mybox.StoresSpec where

import Effectful.Concurrent (forkIO, threadDelay)
import Effectful.Concurrent.QSem
import Effectful.Concurrent.QSemN

import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores

concurrently :: Concurrent :> es => Int -> Eff es () -> Eff es ()
concurrently times act = do
  sem <- newQSemN 0
  void $ replicateM_ 1000 $ forkIO $ do
    act
    signalQSemN sem 1
  waitQSemN sem times

spec :: Spec
spec = withEff runStores $ do
  let store = Store{key = "test-store", def = 0 :: Int}

  it "returns value after set" $ do
    storeSet store 123
    storeGet store >>= (`shouldBe` 123)

  it "returns default value when getting without any set" $ do
    storeGet store >>= (`shouldBe` 0)

  it "modifies values atomically" $ do
    concurrently 1000 $ storeModify store succ
    storeGet store >>= (`shouldBe` 1000)

  describe "storeLock" $
    it "orders effects atomically" $ do
      counter <- newMVar (0 :: Int)
      concurrently 1000 $ storeLock "test" $ do
        -- unsafe without outer lock
        val <- readMVar counter
        threadDelay 10
        modifyMVarPure counter $ const $ succ val
      takeMVar counter >>= (`shouldBe` 1000)

  describe "storeReset" $ do
    it "clears stored values" $ do
      storeSet store 123
      storeReset
      storeGet store >>= (`shouldBe` 0)

    it "maintains locks" $ do
      counter <- newMVar (0 :: Int)
      started <- newQSem 0
      done <- newQSem 0
      -- Start a thread that increments the counter inside a lock
      void $ forkIO $ storeLock "reset-test" $ do
        val <- readMVar counter
        signalQSem started
        threadDelay 1000
        modifyMVarPure counter $ const $ succ val
        signalQSem done
      -- Wait for the thread to start and read the counter
      waitQSem started
      -- Reset stores, which should not remove the lock
      storeReset
      -- Increment the same counter inside the same lock
      storeLock "reset-test" $ modifyMVarPure counter succ
      -- Wait for the first thread to finish
      waitQSem done
      -- Both increments should have happened
      takeMVar counter >>= (`shouldBe` 2)
