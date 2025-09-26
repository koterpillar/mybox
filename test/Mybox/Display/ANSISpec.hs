module Mybox.Display.ANSISpec where

import Mybox.Display
import Mybox.Display.ANSI
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
        displayBanner $ bannerChecking "check" <> bannerInstalling "install"

      output `shouldBe` colourString "<magenta>checking<reset> check\n<blue>installing<reset> install"

    it "replaces banner" $ do
      output <- run $ do
        displayBannerWhile (bannerInstalling "long package name") $ pure ()
        displayBanner $ bannerInstalling "short"

      output `shouldBe` colourString "<blue>installing<reset> short"
