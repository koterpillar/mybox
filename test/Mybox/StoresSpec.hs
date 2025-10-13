module Mybox.StoresSpec where

import Mybox.Prelude
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
    void $ replicateM_ 1000 $ storeModify store succ
    storeGet store >>= (`shouldBe` 1000)
