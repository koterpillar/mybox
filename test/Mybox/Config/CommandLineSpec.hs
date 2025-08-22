module Mybox.Config.CommandLineSpec where

import Options.Applicative

import Mybox.Config.CommandLine
import Mybox.Package.Some
import Mybox.Package.System
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "optionsParser" $ do
    let run = getParseResult . execParserPure defaultPrefs optionsInfo
    it "parses inline packages" $
      run ["--package", "system: test"]
        `shouldBe` Just (CmdInline{inline = [mkSomePackage $ mkSystemPackage "test"], installSet = Nothing})
