module Mybox.Installer.AptSpec where

import Data.Text qualified as Text

import Mybox.Installer.Apt
import Mybox.Installer.Class
import Mybox.SpecBase
import Mybox.Stores

spec :: Spec
spec =
  around withTestEnv $ do
    it "returns installed version for Git" $
      runStores $
        iInstalledVersion Apt "git" >>= (`shouldSatisfy` any ((>= "2.0.0") . Text.replace "1:" ""))
    it "returns latest version for GHC" $ runStores $ do
      v <- iLatestVersion Apt "ghc"
      v `shouldSatisfy` (> "8.")
      v `shouldSatisfy` (< "99")
    it "installed version returns None for non-existent package" $ runStores $ do
      iInstalledVersion Apt "xxxxxxxx" >>= (`shouldBe` Nothing)
    it "latest version fails for non-existent package" $ runStores $ do
      iLatestVersion Apt "xxxxxxxx" `shouldThrow` anyException
