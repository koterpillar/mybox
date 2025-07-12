module Mybox.Installer.FlatpakSpec where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Installer.Flatpak
import Mybox.Installer.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

linux :: OS -> Bool
linux (Linux _) = True
linux _ = False

prerequisites :: Driver :> es => Eff es Bool
prerequisites = do
  os <- drvOS
  case os of
    Linux _ -> do
      docker <- drvRunOutputExit $ shellRaw "echo $DOCKER_IMAGE"
      case docker.output of
        "" -> pure False
        _ -> pure True
    _ -> pure False

expectedFlatpakVersion :: Text -> Bool
expectedFlatpakVersion version = case Text.splitOn ":" version of
  [origin, commit] -> origin `elem` ["fedora", "flathub"] && Text.length commit == 12
  _ -> False

spec :: Spec
spec = onlyIf prerequisites $
  around withTestEnv $ do
    describe "flatpak" $ do
      describe "iLatestVersion" $ do
        it "returns valid version for an existing package" $ do
          iLatestVersion flatpak "org.gnome.Shotwell" >>= (`shouldSatisfy` expectedFlatpakVersion)
        it "fails for non-existent package" $ do
          iLatestVersion flatpak "org.gnome.Shotwell.NonExistent" `shouldThrow` anyException
