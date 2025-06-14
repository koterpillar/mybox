module Mybox.Driver.IOSpec where

import           Data.List.NonEmpty (NonEmpty (..))

import           Test.Hspec

import           Mybox.Driver.Class
import           Mybox.SpecBase

import           System.Exit        (ExitCode (..))

spec :: Spec
spec =
  around withDriver $ do
    it "returns ExitSuccess for true command" $ \drv -> do
      result <- run drv $ drvRunOk $ "true" :| []
      result `shouldBe` ExitSuccess
    it "returns ExitFailure for false command" $ \drv -> do
      result <- run drv $ drvRunOk $ "false" :| []
      result `shouldSatisfy` \case
        ExitSuccess -> False
        ExitFailure _ -> True
    it "captures output from echo command" $ \drv -> do
      result <- run drv $ drvRunOutput $ "echo" :| ["hello", "world"]
      result `shouldBe` "hello world"
    it "handles empty output correctly" $ \drv -> do
      result <- run drv $ drvRunOutput $ "printf" :| [""]
      result `shouldBe` ""
    it "trims whitespace from output" $ \drv -> do
      result <- run drv $ drvRunOutput $ "echo" :| ["  trimmed  "]
      result `shouldBe` "trimmed"
