module Mybox.Package.PipxSpec where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Pipx
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  onlyIf (fmap (== ExitSuccess) $ drvRunOk $ "pipx" :| ["--version"]) $
    describe "remote version" $ do
      around withTestEnv $ do
        it "gets version for existing package" $ do
          let package = PipxPackage "black"
          version <- remoteVersion package
          version `shouldSatisfy` (>= "25.0.0")
          version `shouldSatisfy` (<= "999.0.0")
        it "fails for non-existent package" $ do
          let package = PipxPackage "xxxxxxxxxxxx"
          remoteVersion package `shouldThrow` anyException
        it "returns a Git commit hash for a git package" $ do
          let package = PipxPackage "git+https://github.com/django/django.git"
          remoteVersion package >>= (`shouldSatisfy` (\v -> Text.length v == 40))
  let tqdmPackage name _ =
        ps (PipxPackage name)
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
