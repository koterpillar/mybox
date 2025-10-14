module Mybox.StoresSpec where

import Effectful.Concurrent (forkIO, threadDelay)
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
