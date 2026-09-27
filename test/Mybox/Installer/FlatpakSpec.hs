module Mybox.Installer.FlatpakSpec where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Installer.Flatpak.Internal qualified as Flatpak
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

expectedFlatpakVersion :: Text -> Bool
expectedFlatpakVersion version = Text.length version == 12

flatpak :: Installer
flatpak = Flatpak.flatpak @SystemPackage

spec :: Spec
spec = do
  onlyIfOS "Flatpak installer tests are only available on Linux" (\case Linux _ -> True; _ -> False) $
    skipIf "Flatpak installer tests cannot run in Docker" inDocker $
      withEff (nullTracker . runInstallQueue) $ do
        describe "flatpak" $
          before (ensureInstalled $ Flatpak.flatpakPackage @SystemPackage) $ do
            describe "iLatestVersion" $ do
              it "returns valid version for an existing package" $ do
                iLatestVersion flatpak "org.gnome.Shotwell" >>= (`shouldSatisfy` expectedFlatpakVersion)
              it "fails for non-existent package" $ do
                iLatestVersion flatpak "org.gnome.Shotwell.NonExistent" `shouldThrow` anyException
  describe "mergeVersions" $ do
    it "prefers Flathub" $ do
      let pkg1 = "org.example.Foo"
      let pkg2 = "org.example.Bar"
      let pkg3 = "org.example.Baz"
      let input =
            [ (pkg1, ("alpha", "a1"))
            , (pkg1, (Flatpak.repoName, "f1"))
            , (pkg1, ("omega", "o1"))
            , (pkg2, ("alpha", "a2"))
            , (pkg3, (Flatpak.repoName, "f3"))
            ]
          expected = Map.fromList [(pkg1, "f1"), (pkg2, "a2"), (pkg3, "f3")]
      Flatpak.mergeVersions input `shouldBe` expected
