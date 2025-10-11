module Mybox.Display.OpsSpec where

import Data.Text qualified as Text

import Mybox.Display.Class
import Mybox.Display.Ops
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "tiMerge" $ do
    it "merges two uncoloured items" $ do
      let a = tiMk "hello"
      let b = tiMk " world"
      tiMerge [a, b] `shouldBe` [tiMk "hello world"]
    it "does not merge different coloured items" $ do
      let a = (tiMk "hello"){foreground = Just Red}
      let b = (tiMk " world"){foreground = Just Blue}
      tiMerge [a, b] `shouldBe` [a, b]
    it "does not merge coloured and uncoloured items" $ do
      let a = (tiMk "hello"){foreground = Just Red}
      let b = (tiMk " world"){foreground = Nothing}
      tiMerge [a, b] `shouldBe` [a, b]
