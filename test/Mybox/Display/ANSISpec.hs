module Mybox.Display.ANSISpec where

import Data.Text qualified as Text

import Mybox.Display
import Mybox.Display.ANSI
import Mybox.Display.Print qualified as Print
import Mybox.Display.Tmux
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "runANSIDisplay" $ do
    let run = fmap snd . runTmux . runANSIDisplay @MDisplay
    it "displays logs" $ do
      run (displayLogText "hello") >>= (`shouldBe` "hello")
    it "displays banner" $ do
      output <- run $ displayBanner $ bannerChecking "check" <> bannerInstalling "install"

      output `shouldBe` colourString "<magenta>checking<reset> check\n<blue>installing<reset> install"

    it "displays progress when some packages are already checked" $ do
      output <-
        run $
          displayBanner $
            mconcat
              [ bannerPending "one"
              , bannerPending "two"
              , bannerPending "three"
              , bannerPending "four"
              , bannerUnchanged "one"
              , bannerModified "two"
              ]
      let expectedBar = Text.replicate 38 "█" <> Text.replicate 38 " "
      output `shouldBe` colourString (expectedBar <> " 2/4\n<green>installed<reset> two")

    it "displays final banner when all packages checked" $ do
      output <-
        run $
          displayBanner $
            mconcat
              [ bannerPending "one"
              , bannerPending "two"
              , bannerPending "three"
              , bannerUnchanged "one"
              , bannerModified "two"
              , bannerModified "three"
              ]

      output `shouldBe` colourString "<green>installed<reset> three,two"

    it "places cursor after banner at the end" $ do
      output <- fmap snd . runTmux $ do
        Print.printLn "before"
        runANSIDisplay @MDisplay $ displayBanner $ bannerModified "one"
        Print.printLn "after"

      output `shouldBe` colourString "before\n<green>installed<reset> one\nafter"

    it "replaces banner" $ do
      output <- run $ do
        displayBannerWhile (bannerInstalling "long package name") $ pure ()
        displayBanner $ bannerInstalling "short"

      output `shouldBe` colourString "<blue>installing<reset> short"

    describe "banner wrapping" $ do
      let placeholder :: Int -> Int -> [Text]
          placeholder from to =
            [ "package #" <> Text.pack (show i)
            | i <- [from .. to]
            ]
      let run' = run . displayBanner . mconcat . map bannerInstalling

      it "wraps banner" $ do
        output <- run' $ placeholder 0 9

        output
          `shouldBe` colourString
            ( Text.intercalate "\n" $
                [ "<blue>installing<reset> " <> Text.intercalate "," (placeholder 0 5) <> ","
                , Text.intercalate "," (placeholder 6 9)
                ]
            )

      it "splits very long items" $ do
        let one = "aaa"
        let veryLong = Text.replicate 100 "b"
        let three = "ccc"
        output <- run' [one, veryLong, three]

        let cutoff = 79 - Text.length ("installing " <> one <> ",")

        output
          `shouldBe` colourString
            ( Text.intercalate "\n" $
                [ "<blue>installing<reset> " <> one <> "," <> Text.take cutoff veryLong
                , Text.drop cutoff veryLong <> "," <> three
                ]
            )
