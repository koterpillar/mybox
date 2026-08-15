module Mybox.Compute.JSONPathSpec where

import Mybox.Aeson
import Mybox.Compute.SpecBase
import Mybox.Prelude
import Mybox.Spec.Utils
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "jsonpathProcessor" $ do
    let bars =
          [ object ["bar" .= ("aaaa" :: Text)]
          , object ["bar" .= ("bbbb" :: Text)]
          ]
    let nested = object ["foo" .= bars]
    let restFor base =
          mconcat
            [ "base" .= base
            , "exclude" .= ("bbbb" :: Text)
            ]
    it "extracts values using JSONPath and applies filters" $
      runPureProcessor (String "foo[*].bar") (restFor $ jsonEncode nested) "jsonpath"
        `shouldBe` Just "aaaa"
    it "accepts a path already rooted at $" $
      runPureProcessor (String "$.foo[*].bar") (restFor $ jsonEncode nested) "jsonpath"
        `shouldBe` Just "aaaa"
    it "accepts a path starting with an index" $
      runPureProcessor (String "[*].bar") (restFor $ jsonEncode bars) "jsonpath"
        `shouldBe` Just "aaaa"
    it "fails for an invalid path" $
      evaluate (runPureProcessor (String "foo[") (restFor $ jsonEncode nested) "jsonpath")
        `shouldThrow` stringExceptionContains ["failed to parse query", "unexpected end of input"]
    it "fails for an invalid base" $
      evaluate (runPureProcessor (String "foo[*].bar") (restFor ("not json" :: Text)) "jsonpath")
        `shouldThrow` stringException "Failed to decode base: Unexpected \"not json\", expecting JSON value"
