module Mybox.Compute.URLSpec where

import Mybox.Aeson
import Mybox.Compute.SpecBase
import Mybox.Driver
import Mybox.Prelude
import Mybox.SpecBase

testCurl :: Args -> Maybe Text
testCurl ("curl" :| args) = (\(_, url) -> "content of " <> url <> "\n200") <$> unsnoc args
testCurl _ = Nothing

run :: Value -> Object -> Maybe Value
run value base = runProcessorWith testCurl value base "url"

spec :: Spec
spec = do
  describe "urlProcessor" $ do
    it "fetches URL content" $ do
      let value = String "http://example.com/test"
      run value mempty `shouldBe` Just (String "content of http://example.com/test")
    it "rejects non-string URLs" $ do
      let value = Number 42
      evaluate (run value mempty) `shouldThrow` anyException
