module Mybox.Display.SimpleSpec where

import Mybox.Display
import Mybox.Display.Print (Print)
import Mybox.Display.Print qualified as Print
import Mybox.Display.Simple
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "runSimpleDisplay" $ do
    let run :: Eff '[AppDisplay] () -> String
        run act =
          let ((), logs) =
                runPureEff $
                  Print.runPure $
                    runSimpleDisplay $
                      inject @_ @(AppDisplay : Print : _) act
           in logs
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
          , "checking two"
          , "installing four,three"
          ]
