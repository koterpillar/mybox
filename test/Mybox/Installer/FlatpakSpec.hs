module Mybox.Installer.FlatpakSpec where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Installer.Flatpak
import Mybox.Installer.SpecBase
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

inDocker :: Driver :> es => Eff es Bool
inDocker = do
  r <- drvRunOutputExit (shellRaw "echo $DOCKER")
  pure $ r.output /= ""

expectedFlatpakVersion :: Text -> Bool
expectedFlatpakVersion version = case Text.splitOn ":" version of
  [origin, commit] -> origin `elem` ["fedora", "flathub"] && Text.length commit == 12
  _ -> False

spec :: Spec
spec = onlyIfOS (\case Linux _ -> True; _ -> False) $
  skipIf inDocker $
    around withTestEnv $ do
      describe "flatpak" $
        beforeAll_ (nullTrackerSession $ runInstallQueue $ ensureInstalled flatpakPackage) $ do
          describe "iLatestVersion" $ do
            it "returns valid version for an existing package" $ do
              iLatestVersion flatpak "org.gnome.Shotwell" >>= (`shouldSatisfy` expectedFlatpakVersion)
            it "fails for non-existent package" $ do
              iLatestVersion flatpak "org.gnome.Shotwell.NonExistent" `shouldThrow` anyException
