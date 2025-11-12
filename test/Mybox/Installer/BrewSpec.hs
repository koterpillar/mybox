module Mybox.Installer.BrewSpec where

import Mybox.Driver
import Mybox.Installer.Brew qualified
import Mybox.Installer.Class
import Mybox.Installer.SpecBase
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Package.System
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

onlyMacOS :: (Driver :> es, IOE :> es) => EffSpec es -> EffSpec es
onlyMacOS = onlyIfOS "Homebrew installer tests are only available on macOS" (\case MacOS -> True; _ -> False)

brew :: Installer
brew = Mybox.Installer.Brew.brew @SystemPackage

spec :: Spec
spec = do
  onlyMacOS $ installerSpec brew
  withEff (nullTracker . runInstallQueue) $ do
    it "returns formula version" $ do
      enableSudo
      iLatestVersion brew "the_silver_searcher" >>= (`shouldSatisfy` (\v -> v >= "2.2.0" && v < "99"))
    onlyMacOS $ do
      it "returns cask version" $
        iLatestVersion brew "alacritty" >>= (`shouldSatisfy` (\v -> v >= "0.13.2" && v < "99"))
      it "fails for non-tapped cask" $
        iInstalledVersion brew "homebrew/cask-zzzzzzz/yyyyyyy" `shouldThrow` anyException
