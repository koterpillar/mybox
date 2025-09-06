module Mybox.Display.PrintSpec where

import Mybox.Display.Print qualified as Print
import Mybox.Display.SpecUtils
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
      ((), logs) <- writeHandle $ \h -> Print.run h helloWorld
      logs `shouldBe` "hello\nworld\n"
