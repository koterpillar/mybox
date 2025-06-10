module Mybox.Driver.IOTest where

import           Control.Monad.Reader (runReaderT)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Test.Tasty.HUnit

import           Mybox.Driver.Class
import           Mybox.Driver.IO

import           System.Exit          (ExitCode (..))

localRun :: (forall m. MonadDriver m => m a) -> IO a
localRun action = runReaderT action localDriver

-- Test that "true" command returns ExitSuccess
unit_trueCommandExitSuccess :: IO ()
unit_trueCommandExitSuccess = do
  result <- localRun $ drvRunOk $ "true" :| []
  result @?= ExitSuccess

-- Test that "false" command returns ExitFailure
unit_falseCommandExitFailure :: IO ()
unit_falseCommandExitFailure = do
  result <- localRun $ drvRunOk $ "false" :| []
  case result of
    ExitFailure _ -> assertBool "false command should return ExitFailure" True
    ExitSuccess   -> assertFailure "false command should not return ExitSuccess"

-- Test that we can capture output from echo command
unit_echoOutputCapture :: IO ()
unit_echoOutputCapture = do
  result <- localRun $ drvRunOutput $ "echo" :| ["hello", "world"]
  result @?= "hello world"

-- Test that empty output is handled correctly
unit_emptyOutputCapture :: IO ()
unit_emptyOutputCapture = do
  result <- localRun $ drvRunOutput $ "printf" :| [""]
  result @?= ""

-- Test that whitespace is trimmed from output
unit_outputTrimming :: IO ()
unit_outputTrimming = do
  result <- localRun $ drvRunOutput $ "echo" :| ["  trimmed  "]
  result @?= "trimmed"
