module Mybox.Package.SomeSpec where

import Mybox.Aeson
import Mybox.Package.BrewRepo
import Mybox.Package.Class
import Mybox.Package.Clone
import Mybox.Package.NPM
import Mybox.Package.Pipx
import Mybox.Package.Some
import Mybox.Package.System
import Mybox.Package.URL
import Mybox.Package.YumRepo
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = around withIOEnv $ do
  describe "FromJSON" $ do
    let roundtrip :: Package a => a -> EffSpec '[IOE]
        roundtrip pkg = it ("parses " <> show pkg <> " from JSON") $ do
          let pkgJson = jsonEncode pkg
          let some = jsonDecode @SomePackage "SomePackage" pkgJson
          some `shouldSatisfy` isRight
          some' <- either (error . show) pure some
          show some' `shouldBe` ("SomePackage " <> show pkg)
          let someJson = jsonEncode some'
          someJson `shouldBe` pkgJson
    roundtrip $ mkBrewRepo "test/test"
    roundtrip $ mkClonePackage "ghc/ghc" "ghc"
    roundtrip $ (mkNPMPackage "express"){Mybox.Package.NPM.binaries = ["express"]}
    roundtrip $ mkPipxPackage "pipx"
    roundtrip $ mkSystemPackage "ghc"
    roundtrip $ mkURLPackage "https://example.com/package.tar.gz"
    roundtrip $ mkYumRepo "test" "https://example.com/repo"
