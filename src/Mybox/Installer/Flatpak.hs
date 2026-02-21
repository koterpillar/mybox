{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Installer.Flatpak (flatpak, flatpakPackage) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Effects
import Mybox.Installer.Class
import Mybox.Package.Queue
import Mybox.Prelude

repoName :: Text
repoName = "flathub"

repoUrl :: Text
repoUrl = "https://dl.flathub.org/repo/flathub.flatpakrepo"

flatpakPackage :: IsSystemPackage s => s
flatpakPackage =
  mkSystemPackage_ "flatpak" $
    [ "sudo systemctl daemon-reload"
    , "sudo systemctl is-enabled dbus >/dev/null 2>&1 || sudo systemctl enable --now dbus"
    , shellJoin ["sudo", "flatpak", "remote-add", "--if-not-exists", repoName, repoUrl]
    ]

flatpakInstall :: forall s es. (App es, IsSystemPackage s) => Text -> Eff es ()
flatpakInstall package = do
  queueInstall $ flatpakPackage @s
  drvRun $ "flatpak" :| ["install", "-y", repoName, package]

flatpakUpgrade :: forall s es. (App es, IsSystemPackage s) => Text -> Eff es ()
flatpakUpgrade package = do
  queueInstall $ flatpakPackage @s
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
flatpakPackageInfo :: forall s es. (App es, IsSystemPackage s) => Maybe Text -> Eff es (Map Text PackageVersion)
flatpakPackageInfo _ = do
  queueInstall $ flatpakPackage @s
  iCombineLatestInstalled <$> flatpakGetLatest <*> flatpakGetInstalled

flatpak :: forall s. IsSystemPackage s => Installer
flatpak =
  Installer
    { storeKey = "flatpak"
    , install_ = flatpakInstall @s
    , installURL = iURLNotImplemented
    , upgrade_ = flatpakUpgrade @s
    , getPackageInfo = flatpakPackageInfo @s
    }
