module Mybox.Package.SomeSpec where

import Mybox.Aeson
import Mybox.Package.Archive
import Mybox.Package.BrewRepo
import Mybox.Package.Class
import Mybox.Package.Clone
import Mybox.Package.Daemon
import Mybox.Package.Flatpak
import Mybox.Package.Github
import Mybox.Package.Links
import Mybox.Package.NPM
import Mybox.Package.Pipx
import Mybox.Package.Some
import Mybox.Package.System
import Mybox.Package.URL
import Mybox.Package.YumRepo
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "FromJSON" $ do
    let roundtrip :: Package a => a -> Spec
        roundtrip pkg = it ("parses " <> show pkg <> " from JSON") $ do
          let pkgJson = jsonEncode pkg
          let some = jsonDecode @SomePackage "SomePackage" pkgJson
          some `shouldSatisfy` isRight
          some' <- either (error . show) pure some
          some'.name `shouldBe` pkg.name
          show some' `shouldBe` ("SomePackage " <> show pkg)
          let someJson = jsonEncode some'
          someJson `shouldBe` pkgJson
    roundtrip $ mkBrewRepo "test/test"
    roundtrip $ mkClonePackage "ghc/ghc" $ mkPath "ghc"
    roundtrip $ mkDaemonPackage $ "test-daemon" :| []
    roundtrip $ mkGithubPackage "test/repo"
    roundtrip $ (mkGithubPackage "test/repo"){Mybox.Package.Github.archive = emptyArchiveFields{raw = Right True}}
    roundtrip $ (mkGithubPackage "test/repo"){Mybox.Package.Github.archive = emptyArchiveFields{raw = Left "raw"}}
    roundtrip $ mkFlatpakPackage "com.example.Test"
    roundtrip $ mkLinksPackage (mkPath "src") (mkPath "dest")
    roundtrip $ (mkNPMPackage "express"){Mybox.Package.NPM.binaries = ["express"]}
    roundtrip $ mkPipxPackage "pipx"
    roundtrip $ mkSystemPackage "ghc"
    roundtrip $ mkURLPackage "https://example.com/package.tar.gz"
    roundtrip $ mkYumRepo "test" "https://example.com/repo"

  describe "Eq" $ do
    it "packages with different types are not equal" $ do
      let pkg1 = mkSomePackage $ mkSystemPackage "test"
      let pkg2 = mkSomePackage $ mkBrewRepo "test/test"
      pkg1 `shouldSatisfy` (/= pkg2)

    it "packages of same type with different parameters are not equal" $ do
      let pkg1 = mkSomePackage $ mkSystemPackage "ghc"
      let pkg2 = mkSomePackage $ mkSystemPackage "cabal"
      pkg1 `shouldSatisfy` (/= pkg2)

    it "identical packages are equal" $ do
      let pkg1 = mkSomePackage $ mkSystemPackage "ghc"
      let pkg2 = mkSomePackage $ mkSystemPackage "ghc"
      pkg1 `shouldBe` pkg2
