module Mybox.Package.PipxSpec where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Pipx
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  jsonSpec (Nothing @PipxPackage) [(Nothing, "{\"pipx\": \"django\"}")]
  onlyIf (fmap (== ExitSuccess) $ drvRunOk $ "pipx" :| ["--version"]) $
    describe "remote version" $ do
      around withTestEnv $ do
        it "gets version for existing package" $ nullTrackerSession $ runInstallQueue $ do
          let package = mkPipxPackage "black"
          version <- remoteVersion package
          version `shouldSatisfy` (>= "25.0.0")
          version `shouldSatisfy` (<= "999.0.0")
        it "fails for non-existent package" $ nullTrackerSession $ runInstallQueue $ do
          let package = mkPipxPackage "xxxxxxxxxxxx"
          remoteVersion package `shouldThrow` anyException
        it "returns a Git commit hash for a git package" $ nullTrackerSession $ runInstallQueue $ do
          let package = mkPipxPackage "git+https://github.com/django/django.git"
          remoteVersion package >>= (`shouldSatisfy` (\v -> Text.length v == 40))
  let tqdmPackage name _ =
        ps (mkPipxPackage name)
          & checkInstalledCommandOutput
            ("tqdm" :| ["--help"])
            "Usage:\n  tqdm"
          & ignorePath ".shiv"
          & ignorePath ".local/bin/pipx"
          & ignorePath ".local/share/pipx"
          & ignorePath ".local/state/pipx/log"
          & psPending
  packageSpec $ tqdmPackage "tqdm"
  packageSpec $ tqdmPackage "git+https://github.com/tqdm/tqdm.git"
