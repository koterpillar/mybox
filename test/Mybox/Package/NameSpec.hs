module Mybox.Package.NameSpec where

import Mybox.Aeson
import Mybox.Package.Name
import Mybox.Prelude
import Mybox.SpecBase

data GenericNamePackage = GenericNamePackage
  { name :: Text
  , version :: Text
  , funny :: Bool
  , binaries :: [Text]
  }
  deriving (Eq, Generic, Show)

instance PackageName GenericNamePackage where
  splitName = genericSplitName

instance FromJSON GenericNamePackage

instance ToJSON GenericNamePackage where
  toEncoding = genericToEncoding defaultOptions

data GenericTransferPackage = GenericTransferPackage
  { from :: Text
  , to :: Text
  , param :: Text
  }
  deriving (Eq, Generic, Show)

instance PackageName GenericTransferPackage where
  splitName = genericSplitName' (Just "transfer") $ Proxy @'["from", "to"]

instance FromJSON GenericTransferPackage

instance ToJSON GenericTransferPackage where
  toEncoding = genericToEncoding defaultOptions

spec :: Spec
spec = do
  metaSpec @GenericNamePackage
    [
      ( Nothing
      , "{\"name\": \"tool\", \"version\": \"1.2.3\", \"funny\": false, \"binaries\": [\"tool\"]}"
      )
    ]
  metaSpec @GenericTransferPackage
    [
      ( Nothing
      , "{\"from\": \"here\", \"to\": \"there\", \"param\": \"quickly\"}"
      )
    ]
  describe "genericSplitName" $ do
    describe "single-field name" $ do
      let basePackage = GenericNamePackage{name = "tool", version = "", funny = False, binaries = []}
      it "has accessors" $ do
        basePackage.name `shouldBe` "tool"
        basePackage.version `shouldBe` ""
        basePackage.funny `shouldBe` False
        basePackage.binaries `shouldBe` []
      it "gets name" $ getName basePackage `shouldBe` "tool"
      it "blanks name when there is something left in other fields" $ do
        let package = basePackage{version = "1.2.3", binaries = ["tool"]}
        withoutName package `shouldBe` Just package{name = ""}

      it "returns Nothing when package is fully determined by name" $ do
        withoutName basePackage `shouldBe` Nothing

    describe "multi-field name" $ do
      let baseTransfer = GenericTransferPackage{from = "here", to = "there", param = ""}
      it "has accessors" $ do
        baseTransfer.from `shouldBe` "here"
        baseTransfer.to `shouldBe` "there"
        baseTransfer.param `shouldBe` ""
      it "gets name" $ getName baseTransfer `shouldBe` "transfer#here#there"
      it "blanks all configured name-like fields" $ do
        let package = (baseTransfer{param = "quickly"} :: GenericTransferPackage)
        withoutName package `shouldBe` Just package{from = "", to = ""}

      it "returns Nothing when package is fully determined by configured name fields" $ do
        withoutName baseTransfer `shouldBe` Nothing
