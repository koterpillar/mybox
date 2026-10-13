module Mybox.Package.PythonSpec where

import Data.Text qualified as Text

import Mybox.Package.Class
import Mybox.Package.Python.Internal
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  metaSpec @PythonPackage [(Nothing, "{\"python\": \"django\"}")]
  describe "tool list parsing" $ do
    it "parses tools" $ do
      parseToolList "black v25.1.0\n- black\n- blackd\nmypy v1.9.0\n- mypy\n"
        `shouldBe` [PythonInstalledPackage "black" "25.1.0", PythonInstalledPackage "mypy" "1.9.0"]
    it "is empty without tools" $ do
      parseToolList "No tools installed\n" `shouldBe` []
  describe "repo parsing" $ do
    let pkgRepo = repo . mkPythonPackage
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
      skipGenericLinux "Default installer is unavailable on generic Linux" $
        it "gets version for existing package" $ do
          let package = mkPythonPackage "black"
          version <- remoteVersion package
          version `shouldSatisfy` (>= "25.0.0")
          version `shouldSatisfy` (<= "999.0.0")
      it "fails for non-existent package" $ do
        let package = mkPythonPackage "xxxxxxxxxxxx"
        remoteVersion package `shouldThrow` anyException
      let isGitHash v = Text.length v == 40
      skipGenericLinux "Default installer is unavailable on generic Linux" $
        it "returns a Git commit hash for a git package" $ do
          let package = mkPythonPackage "git+https://github.com/django/django.git"
          remoteVersion package >>= (`shouldSatisfy` isGitHash)
      skipGenericLinux "Default installer is unavailable on generic Linux" $
        it "returns a Git commit hash for a git package with ref" $ do
          let package = mkPythonPackage "git+https://github.com/python/mypy.git@release-1.9"
          remoteVersion package >>= (`shouldSatisfy` isGitHash)
  let psPython =
        ignorePaths [".local" </> "bin" </> "uv", ".local" </> "bin" </> "uvx"]
          . ignorePaths [".cache" </> "uv", ".local" </> "share" </> "uv" </> "python", ".local" </> "share" </> "uv" </> "tools" </> ".gitignore", ".local" </> "share" </> "uv" </> "tools" </> ".lock"]
          . ignorePaths [".rustup" </> "settings.toml"] -- something on GitHub runners creates this
  let tqdmPackage name =
        ps (mkPythonPackage name)
          & checkInstalledCommandOutput
            ("tqdm" :| ["--help"])
            "Usage:\n  tqdm"
          & psPython
  skipGenericLinux "Default installer is unavailable on generic Linux" $ do
    packageSpec $ tqdmPackage "tqdm"
    packageSpec $ tqdmPackage "git+https://github.com/tqdm/tqdm.git"
    packageSpec $
      ps (mkPythonPackage "git+https://github.com/python/mypy.git@release-1.9")
        & checkInstalledCommandOutput
          ("mypy" :| ["--version"])
          "mypy 1.9."
        & psPython
