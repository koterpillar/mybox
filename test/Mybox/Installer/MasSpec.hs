module Mybox.Installer.MasSpec where

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Installer.Mas qualified as Mas
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

onlyMacOS :: (Driver :> es, IOE :> es) => EffSpec es -> EffSpec es
onlyMacOS = onlyIfOS "Mac App Store installer tests are only available on macOS" (\case MacOS -> True; _ -> False)

mas :: Installer
mas = Mas.mas @SystemPackage

spec :: Spec
spec = onlyMacOS $ do
  withEff (nullTracker . runInstallQueue) $ do
    it "returns None for non-installed package" $
      -- DuckDuckGo browser
      iInstalledVersion mas "663592361" >>= (`shouldBe` Nothing)
    it "returns installed app version" $ do
      -- Check for Numbers, a common macOS app
      installed <- iInstalledVersion mas "409203825"
      installed `shouldSatisfy` any ((>=) "14.4")
    it "errors for non-existent package" $
      iInstalledVersion mas "xxxxxxxx" `shouldThrow` anyException
