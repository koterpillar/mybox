module Mybox.Package.PipxSpec where

import Data.Text qualified as Text

import Mybox.Package.Class
import Mybox.Package.Pipx
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  jsonSpec @PipxPackage [(Nothing, "{\"pipx\": \"django\"}")]
  describe "remote version" $ do
    withEff (nullTracker . runInstallQueue) $ do
      it "gets version for existing package" $ do
        let package = mkPipxPackage "black"
        version <- remoteVersion package
        version `shouldSatisfy` (>= "25.0.0")
        version `shouldSatisfy` (<= "999.0.0")
      it "fails for non-existent package" $ do
        let package = mkPipxPackage "xxxxxxxxxxxx"
        remoteVersion package `shouldThrow` anyException
      it "returns a Git commit hash for a git package" $ do
        let package = mkPipxPackage "git+https://github.com/django/django.git"
        remoteVersion package >>= (`shouldSatisfy` (\v -> Text.length v == 40))
  let tqdmPackage name =
        ps (mkPipxPackage name)
          & checkInstalledCommandOutput
            ("tqdm" :| ["--help"])
            "Usage:\n  tqdm"
          & ignorePaths [".shiv", ".local" </> "bin" </> "pipx"]
          & ignorePaths
            [ prefix </> "pipx" </> suffix
            | prefix <- [".local", ".local" </> "share"]
            , suffix <- [".cache", "logs", "py", "shared"]
            ]
          & ignorePaths [".local" </> "state" </> "pipx" </> "log"]
          & ignorePaths [".rustup" </> "settings.toml"] -- something on GitHub runners creates this
  packageSpec $ tqdmPackage "tqdm"
  packageSpec $ tqdmPackage "git+https://github.com/tqdm/tqdm.git"
