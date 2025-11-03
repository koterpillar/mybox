module Mybox.Display.SimpleSpec where

import Mybox.Display
import Mybox.Display.SpecUtils
import Mybox.Prelude
import Mybox.SpecBase

run :: Eff '[AppDisplay] () -> String
run act = let ((), logs) = runPureEff $ runSimpleDisplayPure @MDisplay act in logs

spec :: Spec
spec = do
  describe "runSimpleDisplay" $ do
    it "displays logs" $ do
      let logs = run $ do
            displayLogText "hello"
            displayLogText "world"
      logs `shouldBe` "hello\nworld\n"
    it "displays and updates the banner" $ do
      let logs = run $ do
            displayLogText "hello"
            displayBannerWhile (bannerInstalling "one") $ displayLogText "inside"

            displayLogText "world"
            displayBanner $
              bannerChecking "two" <> bannerInstalling "three" <> bannerInstalling "four"

      logs
        `shouldBe` unlines
          [ "hello"
          , "installing one"
          , "inside"
          , "world"
          , "installing four, three"
          , "checking two"
          ]
