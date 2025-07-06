module Mybox.Installer.SpecBase (Spec, installerSpec) where

import Data.Text (Text)
import Data.Text qualified as Text

import Mybox.Installer.Class
import Mybox.SpecBase
import Mybox.Stores

replaceEpoch :: Text -> Text
replaceEpoch = Text.replace "1:" ""

gitVersion :: Text -> Bool
gitVersion v = replaceEpoch v >= "2.0.0"

ghcVersion :: Text -> Bool
ghcVersion v = v > "8." && v < "99"

installerSpec :: Installer i => i -> Spec
installerSpec i =
  around withTestEnv $ do
    describe "iInstalledVersion" $ do
      it "returns Git version" $
        runStores $
          iInstalledVersion i "git" >>= (`shouldSatisfy` any gitVersion)
      it "returns None for non-installed package" $
        runStores $
          iInstalledVersion i "fish" >>= (`shouldBe` Nothing)
      it "fails for non-existent package" $
        runStores $
          iInstalledVersion i "xxxxxxxx" `shouldThrow` anyException
    describe "iLatestVersion" $ do
      it "returns latest version for GHC" $
        runStores $
          iLatestVersion i "ghc" >>= (`shouldSatisfy` ghcVersion)
      it "fails for non-existent package" $
        runStores $
          iLatestVersion i "xxxxxxxx" `shouldThrow` anyException
