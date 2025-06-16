module Mybox.Driver.IOSpec where

import           Test.Hspec

import           Mybox.Driver.Class
import           Mybox.Prelude
import           Mybox.SpecBase

spec :: Spec
spec =
  around withDriver $ do
    itEff "returns ExitSuccess for true command" $ do
      result <- drvRunOk $ "true" :| []
      liftIO $ result `shouldBe` ExitSuccess
    itEff "returns ExitFailure for false command" $ do
      result <- drvRunOk $ "false" :| []
      liftIO
        $ result `shouldSatisfy` \case
            ExitSuccess -> False
            ExitFailure _ -> True
    itEff "captures output from echo command" $ do
      result <- drvRunOutput $ "echo" :| ["hello", "world"]
      liftIO $ result `shouldBe` "hello world"
    itEff "handles empty output correctly" $ do
      result <- drvRunOutput $ "printf" :| [""]
      liftIO $ result `shouldBe` ""
    itEff "trims whitespace from output" $ do
      result <- drvRunOutput $ "echo" :| ["  trimmed  "]
      liftIO $ result `shouldBe` "trimmed"
