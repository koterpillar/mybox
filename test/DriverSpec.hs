module DriverSpec
  ( spec
  ) where

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
    , coResult :: a
    }
  deriving (Eq, Show)

capture :: IO a -> IO (CapturedOutput a)
capture action = do
  (out, (err, a)) <- Silently.capture $ Silently.hCapture [stderr] action
  return $ CapturedOutput (Text.pack out) (Text.pack err) a

spec :: Spec
spec =
  describe "drvLocal" $ do
    let drv = drvLocal
    describe "drvRunOK" $ do
      it "runs a successful command" $
        capture (drvRunOK ("true" :| []) drv) `shouldReturn`
        CapturedOutput "" "" True
      it "runs a failing command" $
        capture (drvRunOK ("false" :| []) drv) `shouldReturn`
        CapturedOutput "" "" False
    describe "drvRunOutput" $ do
      it "runs a command and captures its output" $
        drvRunOutput ("echo" :| ["hello"]) drv `shouldReturn` "hello"
      it "fails when command fails" $
        drvRunOutput ("false" :| []) drv `shouldThrow`
        (== RunException ("false" :| []))
