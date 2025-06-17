module Mybox.Driver.IOSpec where

import           Mybox.Driver.Class
import           Mybox.Prelude
import           Mybox.SpecBase

spec :: Spec
spec =
  around withTestEnv $ do
    it "returns ExitSuccess for true command" $ do
      result <- drvRunOk $ "true" :| []
      result `shouldBe` ExitSuccess
    it "returns ExitFailure for false command" $ do
      result <- drvRunOk $ "false" :| []
      result `shouldSatisfy` \case
        ExitSuccess -> False
        ExitFailure _ -> True
    it "captures output from echo command" $ do
      result <- drvRunOutput $ "echo" :| ["hello", "world"]
      result `shouldBe` "hello world"
    it "handles empty output correctly" $ do
      result <- drvRunOutput $ "printf" :| [""]
      result `shouldBe` ""
    it "trims whitespace from output" $ do
      result <- drvRunOutput $ "echo" :| ["  trimmed  "]
      result `shouldBe` "trimmed"
