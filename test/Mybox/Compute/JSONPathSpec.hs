module Mybox.Compute.JSONPathSpec where

import Mybox.Aeson
import Mybox.Compute.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "jsonpathProcessor" $ do
    it "extracts values using JSONPath and applies filters" $ do
      let jsonData =
            object
              [ "foo"
                  .= [ object ["bar" .= ("aaaa" :: Text)]
                     , object ["bar" .= ("bbbb" :: Text)]
                     ]
              ]
      let jsonpath = String "foo[*].bar"
      let rest =
            mconcat
              [ "base" .= jsonEncode jsonData
              , "exclude" .= ("bbbb" :: Text)
              ]
      runPureProcessor jsonpath rest "jsonpath" `shouldBe` Just "aaaa"
