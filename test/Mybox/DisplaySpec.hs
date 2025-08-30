module Mybox.DisplaySpec where

import Mybox.Display
import Mybox.Display.Print
import Mybox.Display.Simple
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "runSimpleDisplay" $ do
    let run :: Eff '[AppDisplay] () -> [String]
        run act =
          let ((), logs) =
                runPureEff $
                  runPrintPure $
                    runSimpleDisplay' $
                      inject @_ @(AppDisplay : Print : _) act
           in logs
    it "displays logs" $ do
      let logs = run $ do
            displayLogText "hello"
            displayLogText "world"
      logs `shouldBe` ["hello\n", "world\n"]
    it "displays and updates the banner" $ do
      let logs = run $ do
            displayLogText "hello"
            displayModifyBannerWhile (addInstalling "one") $ displayLogText "inside"
            displayLogText "world"
            displayModifyBanner $
              addChecking "two" . addInstalling "three" . addInstalling "four"
      logs
        `shouldBe` [ "hello\n"
                   , "installing one\n"
                   , "inside\n"
                   , ""
                   , "world\n"
                   , "checking two\ninstalling four,three\n"
                   ]
