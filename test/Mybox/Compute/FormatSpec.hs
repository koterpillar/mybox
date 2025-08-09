module Mybox.Compute.FormatSpec where

import Mybox.Aeson
import Mybox.Compute.Format
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "doFormat" $ do
    it "formats with single placeholder" $ do
      doFormat "Hello {}" ["World"] `shouldBe` Right "Hello World"
    it "formats with multiple placeholders" $ do
      doFormat "one {} two {} three" ["5", "6"] `shouldBe` Right "one 5 two 6 three"
    it "formats with no placeholders" $ do
      doFormat "no placeholders" [] `shouldBe` Right "no placeholders"
    it "formats with adjacent placeholders" $ do
      doFormat "{}{}" ["a", "b"] `shouldBe` Right "ab"
    it "doesn't reuse placeholders" $ do
      doFormat "one {} two {}{} three {}" ["{}", "{", "}", "four"] `shouldBe` Right "one {} two {} three four"
    it "formats with placeholders at start and end" $ do
      doFormat "{}middle{}" ["start", "end"] `shouldBe` Right "startmiddleend"
    it "fails when no more values but placeholders remain" $ do
      doFormat "Hello {} {}" ["World"] `shouldSatisfy` isLeft
    it "fails when no more placeholders but values remain" $ do
      doFormat "Hello" ["World"] `shouldSatisfy` isLeft
    it "fails with multiple excess values" $ do
      doFormat "Hello {}" ["World", "Extra", "More"] `shouldSatisfy` isLeft
    it "fails with multiple missing values" $ do
      doFormat "Hello {} {} {}" ["World"] `shouldSatisfy` isLeft
  describe "formatProcessor" $ do
    it "formats values from JSON" $ do
      let format = String "Hello {} {}"
      let base = "base" .= ["Nice" :: Text, "World"]
      runPureEff (formatProcessor format base) `shouldBe` Just "Hello Nice World"
    it "formats a single value from JSON" $ do
      let format = String "Hello {}"
      let base = "base" .= ("World" :: Text)
      runPureEff (formatProcessor format base) `shouldBe` Just "Hello World"
