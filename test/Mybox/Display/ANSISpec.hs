module Mybox.Display.ANSISpec where

import Mybox.Display
import Mybox.Display.ANSI
import Mybox.Display.Data
import Mybox.Display.Tmux
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "runANSIDisplay" $ do
    let run = fmap snd . runTmux . runANSIDisplay @MDisplay
    it "displays logs" $ do
      run (displayLogText "hello") >>= (`shouldBe` "hello")
    it "displays banner" $ do
      output <- run $ do
        displayModifyBanner $ addChecking "check" . addInstalling "install"
      output `shouldBe` colourString "<blue>checking<reset> check\n<green>installing<reset> install"
    it "replaces banner" $ do
      output <- run $ do
        displayModifyBanner $ addInstalling "long package name"
        displayModifyBanner $ \b -> b{installing = mempty}
        displayModifyBanner $ addInstalling "short"
      output `shouldBe` colourString "<green>installing<reset> short"
