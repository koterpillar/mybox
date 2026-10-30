module Mybox.JSONExSpec where

import Data.Aeson.Types (parseEither)

import Mybox.Aeson
import Mybox.JSONEx
import Mybox.Prelude
import Mybox.SpecBase

data Inner = Inner
  { tags :: [Text]
  , flag :: Bool
  }
  deriving (Eq, Generic, Show)

instance JSONExFields Inner where
  jsonExFields =
    recordFields $
      fieldDefault @"tags" "tag" "inner tags" []
        :* fieldDefault @"flag" "flag" "inner flag" False
        :* Nil

data Outer = Outer
  { name :: Text
  , alias :: Maybe Text
  , comment :: Maybe Text
  , level :: Text
  , inner :: Inner
  }
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via ViaJSONEx Outer

instance JSONEx Outer where
  jsonEx =
    jsonExObject "Outer" $
      field @"name" "renamed" "the name"
        :* fieldOptional @"alias" "alias" "omitted when absent"
        :* fieldNullable @"comment" "comment" "null when absent"
        :* fieldOptionalDefault @"level" "level" "omitted when default" "normal"
        :* fieldFlat @"inner"
        :* Nil

minimal :: Outer
minimal = Outer{name = "n", alias = Nothing, comment = Nothing, level = "normal", inner = Inner{tags = [], flag = False}}

spec :: Spec
spec = do
  describe "jsonExObject" $ do
    it "encodes according to each field's policy" $
      toJSON minimal
        `shouldBe` object
          [ "renamed" .= ("n" :: Text)
          , "comment" .= Null
          , "tag" .= ([] :: [Text])
          , "flag" .= False
          ]
    it "encodes present optional values" $
      toJSON minimal{alias = Just "a", level = "high", inner = Inner{tags = ["x"], flag = True}}
        `shouldBe` object
          [ "renamed" .= ("n" :: Text)
          , "alias" .= ("a" :: Text)
          , "comment" .= Null
          , "level" .= ("high" :: Text)
          , "tag" .= ["x" :: Text]
          , "flag" .= True
          ]
    it "decodes with defaults, treating null as absent" $
      parseEither parseJSON (object ["renamed" .= ("n" :: Text), "alias" .= Null])
        `shouldBe` Right minimal
    it "decodes a single item in place of a list" $
      parseEither parseJSON (object ["renamed" .= ("n" :: Text), "tag" .= ("x" :: Text)])
        `shouldBe` Right minimal{inner = Inner{tags = ["x"], flag = False}}
    it "requires required fields" $
      parseEither (parseJSON @Outer) (object ["alias" .= ("a" :: Text)])
        `shouldBe` Left "Error in $: key \"renamed\" not found"
    it "rejects unknown fields" $
      parseEither (parseJSON @Outer) (object ["renamed" .= ("n" :: Text), "bogus" .= True])
        `shouldBe` Left "Error in $: unexpected keys: \"bogus\""
    it "documents all fields, including flattened ones" $
      (jsonEx @Outer).valueDoc
        `shouldBe` DocObject
          "Outer"
          [ FieldDoc{key = "renamed", description = "the name", required = True, value = DocText}
          , FieldDoc{key = "alias", description = "omitted when absent", required = False, value = DocText}
          , FieldDoc{key = "comment", description = "null when absent", required = False, value = DocText}
          , FieldDoc{key = "level", description = "omitted when default", required = False, value = DocText}
          , FieldDoc{key = "tag", description = "inner tags", required = False, value = DocOneOf [DocText, DocList DocText]}
          , FieldDoc{key = "flag", description = "inner flag", required = False, value = DocBool}
          ]
  describe "enumCodec" $ do
    let codec = enumCodec @Bool "Switch" $ \case
          True -> "on"
          False -> "off"
    it "roundtrips" $
      parseEither codec.parseValue (codec.toValue True) `shouldBe` Right True
    it "rejects unknown values" $
      parseEither codec.parseValue "sideways" `shouldBe` Left "Error in $: Unknown Switch: \"sideways\""
    it "documents all values" $
      codec.valueDoc `shouldBe` DocEnum ["off", "on"]
  describe "NonEmpty" $ do
    it "rejects an empty list" $
      parseEither (jsonEx @(NonEmpty Text)).parseValue (toJSON ([] :: [Text]))
        `shouldBe` Left "Error in $: Expected a non-empty list"
    it "accepts a single item" $
      parseEither (jsonEx @(NonEmpty Text)).parseValue "x" `shouldBe` Right ("x" :| [])
