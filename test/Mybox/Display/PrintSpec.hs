module Mybox.Display.PrintSpec where

import Data.Text qualified as Text
import System.IO

import Mybox.Display.Print qualified as Print
import Mybox.Driver
import Mybox.Prelude
import Mybox.SpecBase

helloWorld :: Print.Print :> es => Eff es ()
helloWorld = do
  Print.printLn "hello"
  Print.printLn "world"
  Print.flush

spec :: Spec
spec = do
  describe "runPure" $ do
    it "collects output" $ do
      let ((), logs) = runPureEff $ Print.runPure helloWorld
      logs `shouldBe` "hello\nworld\n"
  describe "run" $ do
    it "prints to the given handle" $ do
      localDriver $ do
        drvTempFile $ \filePath -> do
          let fileName = Text.unpack filePath.text
          bracket
            (liftIO $ openFile fileName WriteMode)
            (liftIO . hClose)
            $ \h -> Print.run h helloWorld
          liftIO (readFile fileName) >>= (`shouldBe` "hello\nworld\n")
