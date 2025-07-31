module Mybox.Installer.BrewSpec where

import Mybox.Driver
import Mybox.Installer.Brew
import Mybox.Installer.Class
import Mybox.Installer.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

alacrittyVersion :: Text -> Bool
alacrittyVersion v = v >= "0.13.2" && v < "99"

spec :: Spec
spec = onlyIfOS (\case MacOS -> True; _ -> False) $ do
  installerSpec brew
  withTestEff $ do
    it "returns cask version" $
      iLatestVersion brew "alacritty" >>= (`shouldSatisfy` alacrittyVersion)
    it "fails for non-tapped cask" $
      iInstalledVersion brew "homebrew/cask-zzzzzzz/yyyyyyy" `shouldThrow` anyException
