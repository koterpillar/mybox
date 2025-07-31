module Mybox.FiltersSpec where

import Data.Map.Strict qualified as Map

import Mybox.Filters
import Mybox.SpecBase

spec :: Spec
spec = do
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
  describe "fromSynonyms" $ do
    let synonyms =
          Map.fromList @Int
            [ (1, ["1", "one", "hana"])
            , (2, ["2", "two", "dul"])
            ]
    let chooseSyn = choose $ fromSynonyms synonyms 1
    it "chooses synonym matching key" $
      chooseSyn ["one", "four"] `shouldBe` Right "one"
    it "excludes synonyms matching other keys" $
      chooseSyn ["two", "this"] `shouldBe` Right "this"
