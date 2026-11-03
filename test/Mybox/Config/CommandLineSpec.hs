{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mybox.Config.CommandLineSpec where

import Data.Text qualified as Text
import Options.Applicative
import Test.QuickCheck

import Mybox.Config.CommandLine
import Mybox.Prelude
import Mybox.SpecBase

instance Arbitrary CommandLine where
  arbitrary = elements [CLVersion, CLPackageHelp, CommandLine False, CommandLine True]

parsePure :: [Text] -> Maybe CommandLine
parsePure = getParseResult . execParserPure defaultPrefs optionsInfo . map Text.unpack

spec :: Spec
spec = do
  it "returns CLVersion for --version" $
    parsePure ["--version"] `shouldBe` Just CLVersion

  it "rejects --version with --no-self-update" $
    parsePure ["--version", "--no-self-update"] `shouldBe` Nothing

  it "returns CLPackageHelp for --package-help" $
    parsePure ["--package-help"] `shouldBe` Just CLPackageHelp

  it "disables self-update for --no-self-update" $
    parsePure ["--no-self-update"] `shouldBe` Just CommandLine{selfUpdate = False}

  it "enables self-update for no args" $
    parsePure [] `shouldBe` Just CommandLine{selfUpdate = True}

  prop "withoutSelfUpdate is idempotent" $
    \commandLine -> withoutSelfUpdate (withoutSelfUpdate commandLine) === withoutSelfUpdate commandLine

  prop "withoutSelfUpdate preserves informational commands or disables self-update" $
    \commandLine -> case withoutSelfUpdate commandLine of
      CLVersion -> commandLine == CLVersion
      CLPackageHelp -> commandLine == CLPackageHelp
      CommandLine{selfUpdate} -> not selfUpdate

  prop "roundtrips through the command line" $
    \commandLine -> parsePure (commandLineArgs commandLine) == Just commandLine
