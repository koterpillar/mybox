module Mybox.Package.NPMSpec where

import Mybox.Package.Class
import Mybox.Package.NPM
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  metaSpec
    @NPMPackage
    [ (Nothing, "{\"npm\": \"test\"}")
    , (Just "single binary", "{\"npm\": \"test\", \"binary\": \"test\"}")
    , (Just "multiple binaries", "{\"npm\": \"test\", \"binary\": [\"one\", \"two\"]}")
    ]
  describe "remote version" $ do
    withEff (nullTracker . runInstallQueue) $ do
      it "gets version for existing package" $ do
        let package = mkNPMPackage "express"
        version <- remoteVersion package
        version `shouldSatisfy` (>= "4.18.2")
      it "fails for non-existent package" $ do
        let package = mkNPMPackage "xxxxxxxxxxxx"
        remoteVersion package `shouldThrow` anyException
  packageSpec $
    ps ((mkNPMPackage "express-generator"){binaries = ["express"]})
      & checkInstalledCommandOutput
        ("express" :| ["--help"])
        "engine support"
      & ignorePaths [".npm" </> "_cacache", ".npm" </> "_logs", ".npm" </> "_update-notifier-last-checked"]
