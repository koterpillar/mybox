module Mybox.Installer.Flatpak (flatpak, flatpakPackage) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Stores

repoName :: Text
repoName = "flathub"

repoUrl :: Text
repoUrl = "https://dl.flathub.org/repo/flathub.flatpakrepo"

flatpakPackage :: SystemPackage
flatpakPackage =
  (mkSystemPackage "flatpak")
    { post =
        [ shellJoin ["sudo", "systemctl", "daemon-reload"]
        , shellJoin ["sudo", "systemctl", "enable", "--now", "dbus"]
        , shellJoin ["sudo", "flatpak", "remote-add", "--if-not-exists", repoName, repoUrl]
        ]
    }

flatpakInstall :: Driver :> es => Text -> Eff es ()
flatpakInstall package = drvRun $ "flatpak" :| ["install", "-y", repoName, package]

flatpakUpgrade :: Driver :> es => Text -> Eff es ()
flatpakUpgrade package = drvRun $ "flatpak" :| ["upgrade", "-y", package]

parseFlatpakVersions :: Text -> Map Text Text
parseFlatpakVersions output = Map.fromList $ do
  line <- Text.lines output
  case Text.words line of
    [name, origin, commit] -> [(name, origin <> ":" <> commit)]
    _ -> terror $ "Invalid flatpak version output: " <> line

flatpakGetInstalled :: Driver :> es => Eff es (Map Text Text)
flatpakGetInstalled = do
  result <- drvRunOutput $ "flatpak" :| ["list", "--app", "--columns=application,origin,active"]
  pure $ parseFlatpakVersions result

flatpakGetLatest :: Driver :> es => Eff es (Map Text Text)
flatpakGetLatest = do
  result <- drvRunOutput $ "flatpak" :| ["remote-ls", "--app", "--columns=application,origin,commit"]
  pure $ parseFlatpakVersions result

-- FIXME: Cannot selectively query for only a single package
flatpakPackageInfo :: Driver :> es => Maybe Text -> Eff es (Map Text PackageVersion)
flatpakPackageInfo _ = iCombineLatestInstalled <$> flatpakGetLatest <*> flatpakGetInstalled

flatpak :: Installer
flatpak =
  Installer
    { storePackages = jsonStore "flatpak"
    , storeGlobal = jsonStore "flatpak-global"
    , install_ = flatpakInstall
    , upgrade_ = flatpakUpgrade
    , getPackageInfo = flatpakPackageInfo
    }
