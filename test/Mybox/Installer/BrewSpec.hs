module Mybox.Installer.BrewSpec where

import Mybox.Installer.Brew
import Mybox.Installer.Class
import Mybox.Installer.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores

alacrittyVersion :: Text -> Bool
alacrittyVersion v = v >= "0.13.2" && v < "99"

spec :: Spec
spec = installerSpec_ Brew $ do
  it "returns cask version" $
    runStores $
      iLatestVersion Brew "alacritty" >>= (`shouldSatisfy` alacrittyVersion)
  it "fails for non-tapped cask" $
    runStores $
      iInstalledVersion Brew "homebrew/cask-zzzzzzz/yyyyyyy" `shouldThrow` anyException
