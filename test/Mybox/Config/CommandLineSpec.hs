{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mybox.Config.CommandLineSpec where

import Data.Text qualified as Text
import Options.Applicative
import Test.QuickCheck

import Mybox.Config.CommandLine
import Mybox.Prelude
import Mybox.SpecBase

instance Arbitrary CommandLine where
  arbitrary = elements [CLVersion, CommandLine False, CommandLine True]

parsePure :: [Text] -> Maybe CommandLine
parsePure = getParseResult . execParserPure defaultPrefs optionsInfo . map Text.unpack

spec :: Spec
spec = do
  it "returns CLVersion for --version" $
    parsePure ["--version"] `shouldBe` Just CLVersion

  it "rejects --version with --self-update" $
    parsePure ["--version", "--self-update"] `shouldBe` Nothing

  it "enables self-update for --self-update" $
    parsePure ["--self-update"] `shouldBe` Just CommandLine{selfUpdate = True}

  it "returns CommandLine for no args" $
    parsePure [] `shouldBe` Just CommandLine{selfUpdate = False}

  it "removes the self-update option" $
    withoutSelfUpdate (CommandLine True) `shouldBe` CommandLine False

  prop "roundtrips through the command line" $
    \commandLine -> parsePure (commandLineArgs commandLine) == Just commandLine
