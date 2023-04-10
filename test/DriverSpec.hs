module DriverSpec
  ( spec
  ) where

import           Test.Hspec

import           Driver

spec :: Spec
spec =
  describe "drvLocal" $ do
    let drv = drvLocal
    describe "drvRunOK" $ do
      it "runs a successful command" $
        drvRunOK ("true" :| []) drv `shouldReturn` True
      it "runs a failing command" $
        drvRunOK ("false" :| []) drv `shouldReturn` False
    describe "drvRunOutput" $ do
      it "runs a command and captures its output" $
        drvRunOutput ("echo" :| ["hello"]) drv `shouldReturn` "hello"
