module Mybox.Driver.IOSpec where

import           Control.Monad.Reader (runReaderT)

import           Data.List.NonEmpty   (NonEmpty (..))

import qualified Data.Text            as Text

import           Test.Hspec

import           Mybox.Driver.Class
import           Mybox.Driver.IO

import           System.Environment
import           System.Exit          (ExitCode (..))

withDriver :: (IODriver -> IO ()) -> IO ()
withDriver act = do
  image_ <- lookupEnv "DOCKER_IMAGE"
  case image_ of
    Nothing    -> testHostDriver act
    Just image -> dockerDriver (Text.pack image) act

run :: IODriver -> (forall m. MonadDriver m => m a) -> IO a
run drv action = runReaderT action drv

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
