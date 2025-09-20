module Mybox.AesonSpec where

import Mybox.Aeson
import Mybox.Prelude
import Mybox.SpecBase

newtype JATest = JATest Text deriving (Eq, Show)

instance FromJSON JATest where
  parseJSON v =
    JATest
      <$> jsonAlternative
        (withObject "Test 1" (\o -> o .: "foo") v)
        (withObject "Test 2" (\o -> o .: "bar") v)

data WOTTest = WOTTest Text (Maybe Text) deriving (Eq, Show)

instance FromJSON WOTTest where
  parseJSON = withObjectTotal "WOTTest" $ WOTTest <$> takeField "a" <*> takeFieldMaybe "b"

data CLTest = CLTest [Text] deriving (Eq, Show)

instance FromJSON CLTest where
  parseJSON = withObjectTotal "CLTest" $ CLTest <$> takeCollapsedList "items"

type CETest = CollapsedEither Text Int

spec :: Spec
spec = do
  describe "jsonAlternative" $ do
    it "works if first parser succeeds" $
      eitherDecode "{\"foo\": \"test\"}" `shouldBe` Right (JATest "test")
    it "works if second parser succeeds" $
      eitherDecode "{\"bar\": \"test\"}" `shouldBe` Right (JATest "test")
    it "combines errors if both parsers fail" $
      eitherDecode @JATest "{}"
        `shouldBe` Left "Error in $: key \"foo\" not found; key \"bar\" not found"
  describe "withObjectTotal" $ do
    it "fails on missing fields" $
      eitherDecode @WOTTest "{}" `shouldBe` Left "Error in $: key \"a\" not found"
    it "fails on wrong types" $
      eitherDecode @WOTTest "{\"a\": 123}"
        `shouldBe` Left "Error in $.a: parsing Text failed, expected String, but encountered Number"
    it "succeeds on valid JSON" $
      eitherDecode "{\"a\": \"test\", \"b\": \"value\"}" `shouldBe` Right (WOTTest "test" (Just "value"))
    it "succeeds on valid JSON without optional field" $
      eitherDecode "{\"a\": \"test\"}" `shouldBe` Right (WOTTest "test" Nothing)
    it "succeeds when optional field is null" $
      eitherDecode "{\"a\": \"test\", \"b\": null}" `shouldBe` Right (WOTTest "test" Nothing)
    it "fails on extra fields" $
      eitherDecode @WOTTest "{\"a\": \"test\", \"b\": \"value\", \"c\": \"extra\"}"
        `shouldBe` Left "Error in $: unexpected keys: \"c\""
    describe "takeCollapsedList" $ do
      it "returns single element as list" $
        eitherDecode "{\"items\": \"one\"}" `shouldBe` Right (CLTest ["one"])
      it "returns array as list" $
        eitherDecode "{\"items\": [\"one\", \"two\"]}" `shouldBe` Right (CLTest ["one", "two"])
      it "returns empty list when key is missing" $
        eitherDecode "{}" `shouldBe` Right (CLTest [])
      it "fails on wrong type"
        $ shouldBe
          (eitherDecode @CLTest "{\"items\": 123}")
        $ Left "Error in $.items: parsing Text failed, expected String, but encountered Number"
  describe "CollapsedEither" $ do
    it "parses left value" $
      eitherDecode @CETest "\"value\"" `shouldBe` Right (CollapsedEither (Left "value"))
    it "parses right value" $
      eitherDecode @CETest "123" `shouldBe` Right (CollapsedEither (Right 123))
    it "fails when neither parser succeeds"
      $ shouldBe
        (eitherDecode @CETest "true")
      $ Left "Error in $: parsing Int failed, expected Number, but encountered Boolean; parsing Text failed, expected String, but encountered Boolean"
    jsonSpec
      (Nothing @CETest)
      [ (Just "left", "\"left\"")
      , (Just "right", "123")
      ]
