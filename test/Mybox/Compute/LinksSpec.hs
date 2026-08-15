module Mybox.Compute.LinksSpec where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Compute.SpecBase
import Mybox.Driver
import Mybox.Prelude
import Mybox.SpecBase

linksHTML :: Text
linksHTML =
  Text.unlines
    [ "<html>"
    , "    <a href='http://example.com/absolute'>example</a>"
    , "    <a href='/relative'>relative</a>"
    , "    <a href='http://user@other.test/userinfo'>userinfo</a>"
    , "</html>"
    ]

invalidLinkHTML :: Text
invalidLinkHTML = "<html><a href='http://[invalid'>bad</a></html>"

noLinksHTML :: Text
noLinksHTML = "<html><p>nothing to see here</p></html>"

testFetch :: Text -> Maybe Text
testFetch "http://example.com/test" = Just $ linksHTML <> "\n200"
testFetch "http://example.com/invalid-link" = Just $ invalidLinkHTML <> "\n200"
testFetch "http://example.com/no-links" = Just $ noLinksHTML <> "\n200"
testFetch "relative-page" = Just $ linksHTML <> "\n200"
testFetch _ = Nothing

testCurl :: Args -> Maybe Text
testCurl cmd = do
  ("curl" :| args) <- pure cmd
  (_, url) <- unsnoc args
  testFetch url

run :: Value -> Object -> Maybe Value
run value base = runProcessorWith testCurl value base "links"

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
    it "preserves user information in a link" $ do
      let value = String "http://example.com/test"
      let filters = "include" .= String "userinfo"
      run value filters `shouldBe` Just (String "http://user@other.test/userinfo")
    it "errors when links can't be disambiguated" $ do
      let value = String "http://example.com/test"
      evaluate (run value mempty) `shouldThrow` anyException
    it "errors when there are no links" $ do
      let value = String "http://example.com/no-links"
      evaluate (run value mempty)
        `shouldThrow` stringException "No candidates to choose from."
    it "errors for a link that is not a valid URI" $ do
      let value = String "http://example.com/invalid-link"
      evaluate (run value mempty)
        `shouldThrow` stringException "Invalid URI reference: http://[invalid"
    it "errors for a base URL that is not a valid URI" $ do
      let value = String "relative-page"
      let filters = "include" .= String "absolute"
      evaluate (run value filters)
        `shouldThrow` stringException "Invalid URI: relative-page"
