module Mybox.Installer.FlatpakSpec where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Installer.Flatpak qualified as Flatpak
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

expectedFlatpakVersion :: Text -> Bool
expectedFlatpakVersion version = case Text.splitOn ":" version of
  [origin, commit] -> origin `elem` ["fedora", "flathub"] && Text.length commit == 12
  _ -> False

flatpak :: Installer
flatpak = Flatpak.flatpak @SystemPackage

spec :: Spec
spec = onlyIfOS "Flatpak installer tests are only available on Linux" (\case Linux _ -> True; _ -> False) $
  skipIf "Flatpak installer tests cannot run in Docker" inDocker $
    withEff (nullTracker . runInstallQueue) $ do
      describe "flatpak" $
        before (ensureInstalled $ Flatpak.flatpakPackage @SystemPackage) $ do
          describe "iLatestVersion" $ do
            it "returns valid version for an existing package" $ do
              iLatestVersion flatpak "org.gnome.Shotwell" >>= (`shouldSatisfy` expectedFlatpakVersion)
            it "fails for non-existent package" $ do
              iLatestVersion flatpak "org.gnome.Shotwell.NonExistent" `shouldThrow` anyException
