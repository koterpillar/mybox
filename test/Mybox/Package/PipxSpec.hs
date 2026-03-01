module Mybox.Package.PipxSpec where

import Data.Text qualified as Text

import Mybox.Package.Class
import Mybox.Package.Pipx.Internal
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  metaSpec @PipxPackage [(Nothing, "{\"pipx\": \"django\"}")]
  describe "repo parsing" $ do
    let pkgRepo = repo . mkPipxPackage
    it "is Nothing for normal package" $ do
      pkgRepo "black" `shouldBe` Nothing
    it "parses git+https URL" $ do
      pkgRepo "git+https://github.com/django/django" `shouldBe` Just ("https://github.com/django/django", Nothing)
    it "parses git+https URL with ref" $ do
      pkgRepo "git+https://github.com/django/django@release-1.9" `shouldBe` Just ("https://github.com/django/django", Just "release-1.9")
    it "parses git+ssh URL" $ do
      pkgRepo "git+ssh://git@github.com/django/django.git" `shouldBe` Just ("ssh://git@github.com/django/django.git", Nothing)
    it "parses git+ssh URL with ref" $ do
      pkgRepo "git+ssh://git@github.com/django/django.git@release-1.9" `shouldBe` Just ("ssh://git@github.com/django/django.git", Just "release-1.9")
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
      let isGitHash v = Text.length v == 40
      it "returns a Git commit hash for a git package" $ do
        let package = mkPipxPackage "git+https://github.com/django/django.git"
        remoteVersion package >>= (`shouldSatisfy` isGitHash)
      it "returns a Git commit hash for a git package with ref" $ do
        let package = mkPipxPackage "git+https://github.com/python/mypy.git@release-1.9"
        remoteVersion package >>= (`shouldSatisfy` isGitHash)
  let psPython =
        ignorePaths [".shiv", ".local" </> "bin" </> "pipx"]
          . ignorePaths
            [ prefix </> "pipx" </> suffix
            | prefix <- [".local", ".local" </> "share"]
            , suffix <- [".cache", "logs", "py", "shared"]
            ]
          . ignorePaths [".local" </> "state" </> "pipx" </> "log"]
          . ignorePaths [".rustup" </> "settings.toml"] -- something on GitHub runners creates this
  let tqdmPackage name =
        ps (mkPipxPackage name)
          & checkInstalledCommandOutput
            ("tqdm" :| ["--help"])
            "Usage:\n  tqdm"
          & psPython
  packageSpec $ tqdmPackage "tqdm"
  packageSpec $ tqdmPackage "git+https://github.com/tqdm/tqdm.git"
  packageSpec $
    ps (mkPipxPackage "git+https://github.com/python/mypy.git@release-1.9")
      & checkInstalledCommandOutput
        ("mypy" :| ["--version"])
        "mypy 1.9."
      & psPython
