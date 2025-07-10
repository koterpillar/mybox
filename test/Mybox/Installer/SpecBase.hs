module Mybox.Installer.SpecBase (Spec, installerSpec, installerSpec_) where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores

replaceEpoch :: Text -> Text
replaceEpoch = Text.replace "1:" ""

gitVersion :: Text -> Bool
gitVersion v = replaceEpoch v >= "2.0.0"

ghcVersion :: Text -> Bool
ghcVersion v = v > "8." && v < "99"

installerSpec_ :: Installer -> EffSpec '[Driver, Stores, IOE] -> Spec
installerSpec_ i spec =
  around withTestEnv $ do
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
    spec

installerSpec :: Installer -> Spec
installerSpec i = installerSpec_ i $ pure ()
