module Mybox.Compute.BaseSpec where

import Data.Functor.Identity (runIdentity)
import Data.Map qualified as Map

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Prelude
import Mybox.Spec.Utils
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "findSigil" $ do
    it "returns Nothing for object without sigils" $ do
      let obj = mconcat ["key" .= String "value", "other" .= Number 42]
      findSigil obj `shouldBe` Nothing

    it "returns Just for object with one sigil" $ do
      let obj =
            mconcat
              [ "$test" .= String "sigil_value"
              , "key" .= String "normal_value"
              ]
      findSigil obj `shouldBe` Just ("test", String "sigil_value", "key" .= String "normal_value")

    it "throws error for object with multiple sigils" $ do
      let obj = mconcat ["$first" .= String "value1", "$second" .= String "value2"]
      evaluate (findSigil obj) `shouldThrow` errorCallContains ["Multiple sigils found: $first, $second in"]

  describe "processSigils" $ do
    let testProcessor :: Monad m => Processor m
        testProcessor value _ =
          pure $
            Just $
              String $
                "processed:" <> case value of
                  String s -> s
                  _ -> "unknown"

        testSigils = Map.fromList [("test", testProcessor)]
    let run = runIdentity . processSigils testSigils

    describe "for objects" $ do
      it "returns unchanged object when no sigils present" $ do
        let input = object ["key" .= String "value"]
        run input `shouldBe` input

      it "processes object with sigil" $ do
        let input = object ["$test" .= String "hello", "key" .= String "value"]
        run input `shouldBe` String "processed:hello"

      it "throws error for unknown sigil" $ do
        let input = object ["$unknown" .= String "value"]
        evaluate (run input) `shouldThrow` errorCallContains ["Unknown sigil: unknown"]

    describe "for arrays" $ do
      it "processes arrays recursively" $ do
        let input = toJSON [String "hello", object ["$test" .= String "world"]]
        run input `shouldBe` toJSON [String "hello", String "processed:world"]

      it "returns unchanged array when no sigils present" $ do
        let input = toJSON [String "hello", Number 42, Bool True]
        run input `shouldBe` input

    describe "for primitive values" $ do
      for_ [("String", String "test"), ("Number", Number 42), ("Bool", Bool True), ("Null", Null)] $ \(name, value) -> do
        it ("returns unchanged " <> name) $ do
          run value `shouldBe` value

    describe "for nested structures" $ do
      it "processes nested objects with sigils" $ do
        let nested = object ["$test" .= String "nested"]
        let input = object ["outer" .= nested, "other" .= String "value"]
        let expected = object ["outer" .= String "processed:nested", "other" .= String "value"]
        run input `shouldBe` expected

      it "processes mixed nested structures" $ do
        let nestedObj = object ["$test" .= String "obj"]
        let nestedArr = toJSON [object ["$test" .= String "arr"]]
        let input = object ["obj" .= nestedObj, "arr" .= nestedArr]
        let expected = object ["obj" .= String "processed:obj", "arr" .= toJSON [String "processed:arr"]]
        run input `shouldBe` expected
