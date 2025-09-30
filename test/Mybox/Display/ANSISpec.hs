module Mybox.Display.ANSISpec where

import Data.Text qualified as Text

import Mybox.Display
import Mybox.Display.ANSI
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
              , bannerUnchanged "one"
              , bannerModified "two"
              ]
      output `shouldBe` colourString "progress 2/3\n<green>installed<reset> two"

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

    it "replaces banner" $ do
      output <- run $ do
        displayBannerWhile (bannerInstalling "long package name") $ pure ()
        displayBanner $ bannerInstalling "short"

      output `shouldBe` colourString "<blue>installing<reset> short"

    it "wraps banner" $ do
      let placeholder :: Int -> Int -> [Text]
          placeholder from to =
            [ "package #" <> Text.pack (show i)
            | i <- [from .. to]
            ]

      output <-
        run $
          displayBanner $
            mconcat $
              map bannerInstalling $
                placeholder 0 9

      output
        `shouldBe` colourString
          ( Text.intercalate "\n" $
              [ "<blue>installing<reset> " <> Text.intercalate "," (placeholder 0 5) <> ","
              , Text.intercalate "," (placeholder 6 9)
              ]
          )
