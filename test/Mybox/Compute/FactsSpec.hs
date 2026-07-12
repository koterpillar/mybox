module Mybox.Compute.FactsSpec where

import Mybox.Compute.Facts
import Mybox.Compute.SpecBase
import Mybox.Driver
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "osMatches" $ do
    it "matches macOS as darwin" $ do
      runMockPlatform X86_64 MacOS (osMatches ["darwin"]) `shouldBe` True
      runMockPlatform X86_64 (Linux Fedora) (osMatches ["darwin"]) `shouldBe` False

    it "matches any Linux as linux" $ do
      runMockPlatform X86_64 (Linux Fedora) (osMatches ["linux"]) `shouldBe` True
      runMockPlatform X86_64 (Linux (Debian "debian")) (osMatches ["linux"]) `shouldBe` True
      runMockPlatform X86_64 MacOS (osMatches ["linux"]) `shouldBe` False

    it "matches Fedora" $ do
      runMockPlatform X86_64 (Linux Fedora) (osMatches ["fedora"]) `shouldBe` True
      runMockPlatform X86_64 (Linux (Debian "debian")) (osMatches ["fedora"]) `shouldBe` False
      runMockPlatform X86_64 MacOS (osMatches ["fedora"]) `shouldBe` False

    it "matches Debian variants" $ do
      runMockPlatform X86_64 (Linux (Debian "debian")) (osMatches ["debian"]) `shouldBe` True
      runMockPlatform X86_64 (Linux (Debian "ubuntu")) (osMatches ["debian"]) `shouldBe` False
      runMockPlatform X86_64 (Linux Fedora) (osMatches ["debian"]) `shouldBe` False
      runMockPlatform X86_64 MacOS (osMatches ["debian"]) `shouldBe` False

    it "matches any element from the list" $ do
      runMockPlatform X86_64 MacOS (osMatches ["darwin", "debian"]) `shouldBe` True
      runMockPlatform Aarch64 (Linux (Debian "debian")) (osMatches ["darwin", "debian"]) `shouldBe` True
      runMockPlatform Aarch64 (Linux Fedora) (osMatches ["darwin", "debian"]) `shouldBe` False

  describe "architectureMatches" $ do
    it "matches when the architecture is listed" $ do
      runMockPlatform X86_64 MacOS (architectureMatches [X86_64]) `shouldBe` True
      runMockPlatform Aarch64 MacOS (architectureMatches [X86_64]) `shouldBe` False

    it "matches any element from the list" $ do
      runMockPlatform X86_64 MacOS (architectureMatches [X86_64, Aarch64]) `shouldBe` True
      runMockPlatform Aarch64 MacOS (architectureMatches [X86_64, Aarch64]) `shouldBe` True
      runMockPlatform X86_64 MacOS (architectureMatches []) `shouldBe` False

  describe "hostnameMatches" $ do
    it "matches the exact hostname" $ do
      runMockPlatform X86_64 MacOS (hostnameMatches ["my-host"]) `shouldBe` True
      runMockPlatform X86_64 MacOS (hostnameMatches ["other-host"]) `shouldBe` False

    it "matches using a glob" $ do
      runMockPlatform X86_64 MacOS (hostnameMatches ["my-*"]) `shouldBe` True
      runMockPlatform X86_64 MacOS (hostnameMatches ["*-host"]) `shouldBe` True
      runMockPlatform X86_64 MacOS (hostnameMatches ["other-*"]) `shouldBe` False

    it "matches any hostname from the list" $ do
      runMockPlatform X86_64 MacOS (hostnameMatches ["other-host", "my-host"]) `shouldBe` True
      runMockPlatform X86_64 MacOS (hostnameMatches ["a", "b"]) `shouldBe` False
