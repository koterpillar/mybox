module DriverSpec
  ( spec
  ) where

import           Control.Exception  (try)

import           Data.Text          (Text)
import qualified Data.Text          as Text

import           Test.Hspec

import           System.IO          (stderr)
import qualified System.IO.Silently as Silently

import           Driver

data CapturedOutput a =
  CapturedOutput
    { coOut    :: Text
    , coErr    :: Text
    , coResult :: Either RunException a
    }
  deriving (Eq, Show)

capture :: IO a -> IO (CapturedOutput a)
capture action = do
  (out, (err, a)) <- Silently.capture $ Silently.hCapture [stderr] $ try action
  pure $ CapturedOutput (pack out) (pack err) a
  where
    pack = Text.strip . Text.pack

testScript_ :: Bool -> NonEmpty Text
testScript_ succeed =
  shellCmd $
  "echo test-out; echo test-err >&2" <>
  if succeed
    then ""
    else "; false"

testScript :: NonEmpty Text
testScript = testScript_ True

testScriptFail :: NonEmpty Text
testScriptFail = testScript_ False

spec :: Spec
spec =
  describe "drvLocal" $ do
    let drv = drvProcess
    describe "drvRun" $ do
      it "runs a successful command, ignores stdout, shows stderr" $
        capture (drvRun testScript drv) `shouldReturn`
        CapturedOutput "" "test-err" (Right ())
      it "runs a failing command, ignores stdout, shows stderr" $
        capture (drvRun testScriptFail drv) `shouldReturn`
        CapturedOutput "" "test-err" (Left (RunException testScriptFail))
    describe "drvRunOK" $ do
      it "runs a successful command, ignores stdout, shows stderr" $
        capture (drvRunOK testScript drv) `shouldReturn`
        CapturedOutput "" "test-err" (Right True)
      it "runs a failing command, ignores stdout, shows stderr" $
        capture (drvRunOK testScriptFail drv) `shouldReturn`
        CapturedOutput "" "test-err" (Right False)
    describe "drvRunOutput" $ do
      it "runs a successful command, returns stdout, ignores stderr" $
        capture (drvRunOutput testScript drv) `shouldReturn`
        CapturedOutput "" "" (Right "test-out")
      it "fails when command fails, ignores stdout and stderr" $
        capture (drvRunOutput testScriptFail drv) `shouldReturn`
        CapturedOutput "" "" (Left (RunException testScriptFail))
