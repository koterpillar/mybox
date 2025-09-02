module Mybox.Display.TmuxSpec where

import Mybox.Display.Print qualified as Print
import Mybox.Display.Tmux
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "runTmux" $ do
    let run = fmap snd . runTmux . traverse_ Print.print

    it "captures plain strings" $ do
      run ["hello"] >>= (`shouldBe` "hello")

    it "captures coloured strings without extra formatting" $ do
      let mkRed str = "\ESC[31m" <> str <> "\ESC[0m"
      single <- run [mkRed "hello"]
      separate <- run $ map (mkRed . (: [])) "hello"
      single `shouldBe` colourString "<red>hello<reset>"
      single `shouldBe` separate
