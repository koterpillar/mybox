module Mybox.Package.NPMSpec where

import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.NPM
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  jsonSpec
    (Nothing @NPMPackage)
    [ (Nothing, "{\"npm\": \"test\"}")
    , (Just "single binary", "{\"npm\": \"test\", \"binary\": \"test\"}")
    , (Just "multiple binaries", "{\"npm\": \"test\", \"binary\": [\"one\", \"two\"]}")
    ]
  onlyIf (drvExecutableExists "npm") $
    describe "remote version" $ do
      withTestEff $ withEff (nullTrackerSession . runInstallQueue) $ do
        it "gets version for existing package" $ do
          let package = mkNPMPackage "express"
          version <- remoteVersion package
          version `shouldSatisfy` (>= "4.18.2")
        it "fails for non-existent package" $ do
          let package = mkNPMPackage "xxxxxxxxxxxx"
          remoteVersion package `shouldThrow` anyException
  let expressGenerator _ =
        ps ((mkNPMPackage "express-generator"){binaries = ["express"]})
          & checkInstalledCommandOutput
            ("express" :| ["--help"])
            "engine support"
          & ignorePath ".npm"
  packageSpec expressGenerator
