module Mybox.FiltersSpec where

import Mybox.Filters
import Mybox.SpecBase

spec :: Spec
spec = around withIOEnv $ do
  describe "choose" $ do
    let chooseInt = choose @Int
    it "errors with no candidates" $
      chooseInt [] [] `shouldBe` Left []
    it "errors when can't disambiguate" $ do
      chooseInt [(> 0)] [10, 20] `shouldBe` Left [10, 20]
      chooseInt [(< 25), (> 0)] [10, 20, 30] `shouldBe` Left [10, 20]
    it "returns single candidate after elimination" $
      chooseInt [(> 15), \_ -> error "broken filter"] [10, 20] `shouldBe` Right 20
    it "eliminates successively" $
      chooseInt [(< 25), (> 15)] [10, 20, 30] `shouldBe` Right 20
    it "ignores filters that exclude everything" $
      chooseInt [(> 30), (> 15)] [10, 20] `shouldBe` Right 20
