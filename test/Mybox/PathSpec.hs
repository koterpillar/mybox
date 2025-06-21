module Mybox.PathSpec where

import           Mybox.Path
import           Mybox.SpecBase

spec :: Spec
spec =
  around withIOEnv $ do
    describe "pUnder" $ do
      it "returns False for unrelated paths"
        $ pUnder "/usr" "/etc/passwd" `shouldBe` False
      it "returns True for subdirectory paths"
        $ pUnder "/usr" "/usr/local/bin" `shouldBe` True
      it "returns True for the same path" $ pUnder "/usr" "/usr" `shouldBe` True
      it "returns False for prefix paths"
        $ pUnder "/usr/local" "/usr" `shouldBe` False
      it "returns False for string-prefix paths"
        $ pUnder "/usr" "/usr1" `shouldBe` False
