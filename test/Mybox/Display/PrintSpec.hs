module Mybox.Display.PrintSpec where

import Mybox.Display.Print qualified as Print
import Mybox.Display.SpecUtils
import Mybox.Display.Tmux
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
    it "stubs terminal size" $ do
      let (sz, logs) = runPureEff $ Print.runPure Print.terminalSize
      logs `shouldBe` ""
      sz `shouldBe` Nothing
  describe "run" $ do
    it "prints to the given handle" $ do
      ((), logs) <- writeHandle $ \h -> Print.run h helloWorld
      logs `shouldBe` "hello\nworld\n"
    it "gets terminal size" $ do
      (sz, logs) <- runTmux $ Print.terminalSize
      logs `shouldBe` ""
      sz `shouldBe` Just (24, 80)
