module Mybox.Package.NPMSpec where

import Data.Map qualified as Map

import Mybox.Aeson
import Mybox.Package.Class
import Mybox.Package.NPM.Internal
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.Spec.Utils
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  metaSpec
    @NPMPackage
    [(Nothing, "{\"npm\": \"test\"}")]
  describe "remote version" $ do
    withEff (nullTracker . runInstallQueue) $ do
      skipGenericLinux "Default installer is unavailable on generic Linux" $
        it "gets version for existing package" $ do
          let package = mkNPMPackage "express"
          version <- remoteVersion package
          version `shouldSatisfy` (>= "4.18.2")
      it "fails for non-existent package" $ do
        let package = mkNPMPackage "xxxxxxxxxxxx"
        remoteVersion package `shouldThrow` anyException
  skipGenericLinux "Default installer is unavailable on generic Linux" $
    packageSpec $
      ps (mkNPMPackage "express-generator")
        & checkInstalledCommandOutput
          ("express" :| ["--help"])
          "engine support"
        & ignorePaths [".npm" </> "_cacache", ".npm" </> "_logs", ".npm" </> "_update-notifier-last-checked"]
  describe "PackageJson" $ do
    it "parses bin field" $ do
      let json = "{\"bin\": {\"foo\": \"bar\"}}"
      let resultE = jsonDecode "example" json
      resultE `shouldSatisfy` isRight
      let pkg = requireRight resultE
      pkg `shouldBe` PackageJson (Map.singleton "foo" "bar")
    it "parses empty object" $ do
      let json = "{}"
      let resultE = jsonDecode "example" json
      resultE `shouldSatisfy` isRight
      let pkg = requireRight resultE
      pkg `shouldBe` PackageJson Map.empty
