module Mybox.Display.SimpleSpec where

import Data.Version

import Mybox.Display
import Mybox.Display.SpecUtils
import Mybox.JSONCodec
import Mybox.Prelude
import Mybox.SpecBase

run :: Eff '[AppDisplay] () -> String
run act = let ((), logs) = runPureEff $ runSimpleDisplayPure @MDisplay act in logs

spec :: Spec
spec = do
  describe "runSimpleDisplay" $ do
    it "displays logs" $ do
      let logs = run $ do
            displayLogText "hello"
            displayLogText "world"
      logs `shouldBe` "hello\nworld\n"
    it "displays version" $ do
      let logs = run $ displayVersion $ makeVersion [1, 2, 3]
      logs `shouldBe` "mybox 1.2.3\n"
    it "displays documentation" $ do
      let fieldDoc key description required value = FieldDoc{key, description, required, value}
          doc =
            DocOneOf
              [ DocObject
                  "Shape"
                  [ fieldDoc "name" "The name" True DocText
                  , fieldDoc "sides" "How many sides" False (DocOneOf [DocText, DocList DocText])
                  , fieldDoc "fill" "Fill pattern" False (DocObject "Fill" [fieldDoc "solid" "Solid fill" False DocBool])
                  ]
              , DocObject "Dot" []
              ]
      let logs = run $ displayDoc doc
      logs
        `shouldBe` unlines
          [ "Shape"
          , "  * name   text           The name"
          , "    sides  text | [text]  How many sides"
          , "    fill   Fill           Fill pattern"
          , "      solid  boolean  Solid fill"
          , ""
          , "Dot"
          ]
    it "displays and updates the banner" $ do
      let logs = run $ do
            displayLogText "hello"
            displayBannerWhile (bannerInstalling "one") $ displayLogText "inside"

            displayLogText "world"
            displayBanner $
              bannerChecking "two" <> bannerInstalling "three" <> bannerInstalling "four"

      logs
        `shouldBe` unlines
          [ "hello"
          , "installing one"
          , "inside"
          , "world"
          , "installing four, three"
          , "checking two"
          ]
