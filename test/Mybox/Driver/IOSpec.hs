module Mybox.Driver.IOSpec where

import           Test.Hspec

import           Mybox.Driver.Class
import           Mybox.Prelude
import           Mybox.SpecBase

spec :: Spec
spec =
  around withDriver $ do
    it "returns ExitSuccess for true command" $ \(WD drv) -> do
      result <- drv $ drvRunOk $ "true" :| []
      result `shouldBe` ExitSuccess
    it "returns ExitFailure for false command" $ \(WD drv) -> do
      result <- drv $ drvRunOk $ "false" :| []
      result `shouldSatisfy` \case
        ExitSuccess -> False
        ExitFailure _ -> True
    it "captures output from echo command" $ \(WD drv) -> do
      result <- drv $ drvRunOutput $ "echo" :| ["hello", "world"]
      result `shouldBe` "hello world"
    it "handles empty output correctly" $ \(WD drv) -> do
      result <- drv $ drvRunOutput $ "printf" :| [""]
      result `shouldBe` ""
    it "trims whitespace from output" $ \(WD drv) -> do
      result <- drv $ drvRunOutput $ "echo" :| ["  trimmed  "]
      result `shouldBe` "trimmed"
