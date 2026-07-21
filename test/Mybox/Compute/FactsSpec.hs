module Mybox.Compute.FactsSpec where

import Mybox.Compute.Facts
import Mybox.Compute.SpecBase
import Mybox.Driver
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "osMatch" $ do
    it "matches macOS as darwin" $ do
      runMockPlatform X86_64 MacOS (osMatch "darwin") `shouldBe` True
      runMockPlatform X86_64 (Linux Fedora) (osMatch "darwin") `shouldBe` False

    it "matches any Linux as linux" $ do
      runMockPlatform X86_64 (Linux Fedora) (osMatch "linux") `shouldBe` True
      runMockPlatform X86_64 (Linux (Debian "debian")) (osMatch "linux") `shouldBe` True
      runMockPlatform X86_64 MacOS (osMatch "linux") `shouldBe` False

    it "matches Fedora" $ do
      runMockPlatform X86_64 (Linux Fedora) (osMatch "fedora") `shouldBe` True
      runMockPlatform X86_64 (Linux (Debian "debian")) (osMatch "fedora") `shouldBe` False
      runMockPlatform X86_64 MacOS (osMatch "fedora") `shouldBe` False

    it "matches Debian variants" $ do
      runMockPlatform X86_64 (Linux (Debian "debian")) (osMatch "debian") `shouldBe` True
      runMockPlatform X86_64 (Linux (Debian "ubuntu")) (osMatch "debian") `shouldBe` False
      runMockPlatform X86_64 (Linux Fedora) (osMatch "debian") `shouldBe` False
      runMockPlatform X86_64 MacOS (osMatch "debian") `shouldBe` False

  describe "architectureMatches" $ do
    it "matches the architecture" $ do
      runMockPlatform X86_64 MacOS (architectureMatches "x86_64") `shouldBe` True
      runMockPlatform Aarch64 MacOS (architectureMatches "x86_64") `shouldBe` False

  describe "hostnameMatches" $ do
    it "matches the exact hostname" $ do
      runMockPlatform X86_64 MacOS (hostnameMatches "my-host") `shouldBe` True
      runMockPlatform X86_64 MacOS (hostnameMatches "other-host") `shouldBe` False

    it "matches using a glob" $ do
      runMockPlatform X86_64 MacOS (hostnameMatches "my-*") `shouldBe` True
      runMockPlatform X86_64 MacOS (hostnameMatches "*-host") `shouldBe` True
      runMockPlatform X86_64 MacOS (hostnameMatches "other-*") `shouldBe` False
