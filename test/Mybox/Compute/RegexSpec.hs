module Mybox.Compute.RegexSpec where

import Mybox.Aeson
import Mybox.Compute.Regex
import Mybox.Prelude
import Mybox.SpecBase

installScript :: Text
installScript =
  "#!/bin/sh\nDEFAULT_VERSION=\"1.2.3\"\nOTHER_VERSION=\"4.5.6\"\n"

spec :: Spec
spec = do
  describe "regexProcessor" $ do
    it "extracts the capture group" $ do
      let pattern_ = String "^DEFAULT_VERSION=\"([^\"]*)\""
      let rest = "base" .= installScript
      runPureEff (regexProcessor pattern_ rest) `shouldBe` Just "1.2.3"
    it "extracts the whole match without capture groups" $ do
      let pattern_ = String "^DEFAULT_VERSION=\"[^\"]*\""
      let rest = "base" .= installScript
      runPureEff (regexProcessor pattern_ rest)
        `shouldBe` Just "DEFAULT_VERSION=\"1.2.3\""
    it "applies filters to multiple matches" $ do
      let pattern_ = String "[0-9]+\\.[0-9]+\\.[0-9]+"
      let rest =
            mconcat
              [ "base" .= installScript
              , "exclude" .= ("1.2.3" :: Text)
              ]
      runPureEff (regexProcessor pattern_ rest) `shouldBe` Just "4.5.6"
    it "rejects ambiguous matches" $ do
      let pattern_ = String "[0-9]+\\.[0-9]+\\.[0-9]+"
      let rest = "base" .= installScript
      evaluate (runPureEff $ regexProcessor pattern_ rest) `shouldThrow` anyException
    it "rejects no matches" $ do
      let pattern_ = String "^NO_SUCH_VERSION=\"([^\"]*)\""
      let rest = "base" .= installScript
      evaluate (runPureEff $ regexProcessor pattern_ rest) `shouldThrow` anyException
    it "rejects invalid patterns" $ do
      let pattern_ = String "("
      let rest = "base" .= installScript
      evaluate (runPureEff $ regexProcessor pattern_ rest) `shouldThrow` anyException
