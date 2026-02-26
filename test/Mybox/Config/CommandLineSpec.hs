module Mybox.Config.CommandLineSpec where

import Options.Applicative

import Mybox.Config.CommandLine
import Mybox.SpecBase

spec :: Spec
spec = do
  let parsePure :: [String] -> Maybe CommandLine
      parsePure = getParseResult . execParserPure defaultPrefs optionsInfo

  it "returns CLVersion for --version" $ do
    let result = parsePure ["--version"]
    result `shouldBe` Just CLVersion

  it "returns CommandLine for no args" $ do
    let result = parsePure []
    result `shouldBe` Just CommandLine
