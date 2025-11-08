module Mybox.Installer.Flatpak (flatpak, flatpakPackage) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Effects
import Mybox.Installer.Class
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude

repoName :: Text
repoName = "flathub"

repoUrl :: Text
repoUrl = "https://dl.flathub.org/repo/flathub.flatpakrepo"

flatpakPackage :: SystemPackage
flatpakPackage =
  (mkSystemPackage "flatpak")
    { post =
        map
          (shellJoin . sudo)
          [ "systemctl" :| ["daemon-reload"]
          , "systemctl" :| ["enable", "--now", "dbus"]
          , "flatpak" :| ["remote-add", "--if-not-exists", repoName, repoUrl]
          ]
    }

flatpakInstall :: App es => Text -> Eff es ()
flatpakInstall package = do
  queueInstall flatpakPackage
  drvRun $ "flatpak" :| ["install", "-y", repoName, package]

flatpakUpgrade :: App es => Text -> Eff es ()
flatpakUpgrade package = do
  queueInstall flatpakPackage
  drvRun $ "flatpak" :| ["upgrade", "-y", package]

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
flatpakPackageInfo :: App es => Maybe Text -> Eff es (Map Text PackageVersion)
flatpakPackageInfo _ = do
  queueInstall flatpakPackage
  iCombineLatestInstalled <$> flatpakGetLatest <*> flatpakGetInstalled

flatpak :: Installer
flatpak =
  Installer
    { storeKey = "flatpak"
    , install_ = flatpakInstall
    , installURL = iURLNotImplemented
    , upgrade_ = flatpakUpgrade
    , getPackageInfo = flatpakPackageInfo
    }
