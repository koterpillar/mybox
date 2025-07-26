module Mybox.StoresSpec where

import Data.Map qualified as Map

import Mybox.SpecBase
import Mybox.Stores

spec :: Spec
spec = withEff runStores $ do
  let store = mkStore "test-store" textIso $ jsonIso @Int

  it "returns value after set" $ do
    storeSet store "key1" 123
    storeGet store "key1" >>= (`shouldBe` Just 123)

  it "returns Nothing when getting without any set" $ do
    storeGet store "nonexistent-key" >>= (`shouldBe` Nothing)

  it "returns Nothing after set and delete" $ do
    storeSet store "key2" 456
    storeDelete store "key2"
    storeGet store "key2" >>= (`shouldBe` Nothing)

  it "gets all values" $ do
    storeSet store "key1" 123
    storeSet store "key2" 456
    storeGetAll store >>= (`shouldBe` Map.fromList [("key1", 123), ("key2", 456)])

  it "clears values" $ do
    storeSet store "key1" 123
    storeSet store "key2" 456
    storeClear store
    storeGet store "key1" >>= (`shouldBe` Nothing)
    storeGetAll store >>= (`shouldBe` Map.empty)
