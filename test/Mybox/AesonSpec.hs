module Mybox.AesonSpec where

import Mybox.Aeson
import Mybox.Prelude
import Mybox.SpecBase

newtype Test = Test {unTest :: Text} deriving (Eq, Show)

instance FromJSON Test where
  parseJSON v =
    Test
      <$> jsonAlternative
        (withObject "Test 1" (\o -> o .: "foo") v)
        (withObject "Test 2" (\o -> o .: "bar") v)

spec :: Spec
spec =
  around withIOEnv $ do
    describe "jsonAlternative" $ do
      it "works if first parser succeeds" $
        eitherDecode "{\"foo\": \"test\"}" `shouldBe` Right (Test "test")
      it "works if second parser succeeds" $
        eitherDecode "{\"bar\": \"test\"}" `shouldBe` Right (Test "test")
      it "combines errors if both parsers fail" $
        eitherDecode @Test "{}"
          `shouldBe` Left "Error in $: key \"foo\" not found; key \"bar\" not found"
