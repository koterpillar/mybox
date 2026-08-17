module Mybox.Package.NPMSpec where

import Data.Set qualified as Set

import Mybox.Aeson
import Mybox.Package.Class
import Mybox.Package.NPM.Internal
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.Release
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
          release <- selectedRelease package
          release.version `shouldSatisfy` (>= "4.18.2")
      it "fails for non-existent package" $ do
        let package = mkNPMPackage "xxxxxxxxxxxx"
        selectedRelease package `shouldThrow` anyException
  skipGenericLinux "Default installer is unavailable on generic Linux" $ do
    let npmPaths = [".npm" </> "_cacache", ".npm" </> "_logs", ".npm" </> "_update-notifier-last-checked"]
    packageSpec $
      ps (mkNPMPackage "express-generator")
        & checkInstalledCommandOutput
          ("express" :| ["--help"])
          "engine support"
        & ignorePaths npmPaths
    packageSpec $
      ps (mkNPMPackage "yarn-deduplicate")
        & checkInstalledCommandOutput
          ("yarn-deduplicate" :| ["--help"])
          "Usage: yarn-deduplicate"
        & ignorePaths npmPaths
    packageSpec $
      ps (mkNPMPackage "@kilocode/cli")
        & checkInstalledCommandOutput
          ("kilo" :| ["help"])
          "show token usage"
        & ignorePaths (npmPaths <> [".local" </> "share" </> "kilo", ".local" </> "state" </> "kilo"])
  describe "PackageJsonBin" $ do
    it "parses bin field" $ do
      let json = "{\"bin\": {\"foo\": \"bar\"}}"
      let resultE = jsonDecode "example" json
      resultE `shouldSatisfy` isRight
      let pkg = requireRight resultE
      pkg `shouldBe` PackageJsonBin (Right (Set.singleton "foo"))
    it "parses raw string" $ do
      let json = "{\"bin\": \"foo\"}"
      let resultE = jsonDecode "example" json
      resultE `shouldSatisfy` isRight
      let pkg = requireRight resultE
      pkg `shouldBe` PackageJsonBin (Left ())
    it "parses empty object" $ do
      let json = "{}"
      let resultE = jsonDecode "example" json
      resultE `shouldSatisfy` isRight
      let pkg = requireRight resultE
      pkg `shouldBe` PackageJsonBin (Right mempty)
  describe "binary naming" $ do
    let unscoped = mkNPMPackage "foo"
    let scoped = mkNPMPackage "@foo/bar"
    let singleBin = PackageJsonBin (Left ())
    let multiBin = PackageJsonBin (Right (Set.singleton "baz"))
    it "uses package name for non-scoped package when bin is raw string" $ do
      binariesFromPackageJson unscoped singleBin `shouldBe` Set.singleton "foo"
    it "uses package basename for scoped package when bin is raw string" $ do
      binariesFromPackageJson scoped singleBin `shouldBe` Set.singleton "bar"
    it "uses map keys when bin is object" $ do
      binariesFromPackageJson scoped multiBin `shouldBe` Set.singleton "baz"
