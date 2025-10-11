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

  describe "tiProgressBar" $ do
    it "returns Nothing when total is 0" $ do
      tiProgressBar (Just 10) 5 0 `shouldBe` Nothing

    it "returns Nothing when finished" $ do
      tiProgressBar (Just 10) 10 10 `shouldBe` Nothing
      tiProgressBar (Just 10) 15 10 `shouldBe` Nothing

    let mkBar progress = tiMerge <$> tiProgressBar (Just 15) progress 80

    it "draws progress bar for 0%" $ do
      mkBar 0
        `shouldBe` Just
          [ tiMk $ Text.replicate 10 " " <> "  0/80"
          ]

    it "draws progress bar for 50%" $ do
      mkBar 40
        `shouldBe` Just
          [ tiMk $ Text.replicate 5 "█" <> Text.replicate 5 " " <> " 40/80"
          ]

    it "draws progress bar with a partial fill" $ do
      mkBar 33
        `shouldBe` Just
          [ tiMk $ Text.replicate 4 "█" <> "▏" <> Text.replicate 5 " " <> " 33/80"
          ]
