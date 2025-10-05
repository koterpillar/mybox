module Mybox.Compute.LinksSpec where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Compute.Links
import Mybox.Driver
import Mybox.Driver.Test
import Mybox.Prelude
import Mybox.SpecBase

linksHTML :: Text
linksHTML =
  Text.unlines
    [ "<html>"
    , "    <a href='http://example.com/absolute'>example</a>"
    , "    <a href='/relative'>relative</a>"
    , "</html>"
    ]

testFetch :: Text -> Maybe Text
testFetch "http://example.com/test" = Just $ linksHTML <> "\n200"
testFetch _ = Nothing

testCurl :: Args -> Maybe Text
testCurl cmd = do
  ("curl" :| args) <- pure cmd
  (_, url) <- unsnoc args
  testFetch url

run :: Value -> Object -> Maybe Value
run value base = runPureEff $ pureDriver testCurl $ linksProcessor value base

spec :: Spec
spec = do
  describe "linksProcessor" $ do
    it "fetches HTML and extracts links" $ do
      let value = String "http://example.com/test"
      let filters = "include" .= String "example"
      run value filters `shouldBe` Just (String "http://example.com/absolute")
    it "fetches a relative link" $ do
      let value = String "http://example.com/test"
      let filters = "include" .= String "relative"
      run value filters `shouldBe` Just (String "http://example.com/relative")
    it "errors when links can't be disambiguated" $ do
      let value = String "http://example.com/test"
      evaluate (run value mempty) `shouldThrow` anyException
