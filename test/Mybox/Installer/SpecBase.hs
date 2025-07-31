module Mybox.Installer.SpecBase (installerSpec) where

import Data.Text qualified as Text

import Mybox.Installer.Class
import Mybox.Prelude
import Mybox.SpecBase

replaceEpoch :: Text -> Text
replaceEpoch = Text.replace "1:" ""

gitVersion :: Text -> Bool
gitVersion v = replaceEpoch v >= "2.0.0"

ghcVersion :: Text -> Bool
ghcVersion v = v > "8." && v < "99"

installerSpec :: Installer -> Spec
installerSpec i =
  withTestEff $ do
    describe "iInstalledVersion" $ do
      it "returns Git version" $
        iInstalledVersion i "git" >>= (`shouldSatisfy` any gitVersion)
      it "returns None for non-installed package" $
        iInstalledVersion i "fish" >>= (`shouldBe` Nothing)
      it "fails for non-existent package" $
        iInstalledVersion i "xxxxxxxx" `shouldThrow` anyException
    describe "iLatestVersion" $ do
      it "returns latest version for GHC" $
        iLatestVersion i "ghc" >>= (`shouldSatisfy` ghcVersion)
      it "fails for non-existent package" $
        iLatestVersion i "xxxxxxxx" `shouldThrow` anyException
