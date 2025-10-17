module Mybox.StoresSpec where

import Effectful.Concurrent (threadDelay)

import Mybox.Prelude
import Mybox.Spec.Utils
import Mybox.SpecBase
import Mybox.Stores

spec :: Spec
spec = withEff runStores $ do
  let store = Store{key = "test-store", def = 0 :: Int}

  it "returns value after set" $ do
    storeSet store 123
    storeGet store >>= (`shouldBe` 123)

  it "returns default value when getting without any set" $ do
    storeGet store >>= (`shouldBe` 0)

  it "modifies values atomically" $ do
    concurrently_ 1000 $ storeModify store succ
    storeGet store >>= (`shouldBe` 1000)

  describe "storeLock" $
    it "orders effects atomically" $ do
      counter <- newMVar (0 :: Int)
      concurrently_ 1000 $ storeLock "test" $ do
        -- unsafe without outer lock
        val <- readMVar counter
        threadDelay 10
        modifyMVarPure counter $ const $ succ val
      takeMVar counter >>= (`shouldBe` 1000)
