module Mybox.Package.SomeSpec where

import Mybox.Aeson
import Mybox.Package.Class
import Mybox.Package.Clone
import Mybox.Package.NPM
import Mybox.Package.Pipx
import Mybox.Package.Some
import Mybox.Package.System
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
    roundtrip $ ClonePackage{repo = "ghc/ghc", branch = Nothing, destination = "ghc"}
    roundtrip $ NPMPackage{package = "express", binaries = ["express"]}
    roundtrip $ PipxPackage{package = "pipx"}
    roundtrip $ SystemPackage{name = "ghc", url = Nothing, autoUpdates = False}
