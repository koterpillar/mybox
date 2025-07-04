module Mybox.Package.NPMSpec where

import Mybox.Package.Class
import Mybox.Package.NPM
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "remote version" $ do
    around withTestEnv $ do
      xit "gets version for existing package" $ do
        let package = NPMPackage "express" []
        version <- remoteVersion package
        version `shouldSatisfy` (>= "4.18.2")
      it "fails for non-existent package" $ do
        let package = NPMPackage "xxxxxxxxxxxx" []
        remoteVersion package `shouldThrow` anyException
  let expressGenerator _ =
        ps (NPMPackage "express-generator" ["express"])
          & checkInstalledCommandOutput
            ("express" :| ["--help"])
            "engine support"
          & ignorePath ".npm"
          & psPending
  packageSpec expressGenerator
