module Mybox.StoresSpec where

import Mybox.SpecBase
import Mybox.Stores

spec :: Spec
spec =
  around withIOEnv $ do
    let store = textStore "test-store"

    it "returns value after set" $ do
      result <- runStores $ do
        storeSet store "key1" "value1"
        storeGet store "key1"
      result `shouldBe` Just "value1"

    it "returns Nothing when getting without any set" $ do
      result <- runStores $ do
        storeGet store "nonexistent-key"
      result `shouldBe` Nothing

    it "returns Nothing after set and delete" $ do
      result <- runStores $ do
        storeSet store "key2" "value2"
        storeDelete store "key2"
        storeGet store "key2"
      result `shouldBe` Nothing
