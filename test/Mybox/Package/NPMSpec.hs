module Mybox.Package.NPMSpec where

import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.NPM
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    (Nothing @NPMPackage)
    [ (Nothing, "{\"npm\": \"test\"}")
    , (Just "single binary", "{\"npm\": \"test\", \"binary\": \"test\"}")
    , (Just "multiple binaries", "{\"npm\": \"test\", \"binary\": [\"one\", \"two\"]}")
    ]
  onlyIf (fmap (== ExitSuccess) $ drvRunOk $ "npm" :| ["--version"]) $
    describe "remote version" $ do
      around withTestEnv $ do
        it "gets version for existing package" $ do
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
