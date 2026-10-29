module Mybox.Installer.MacOS (macOS) where

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Prelude

pApplications :: Path Abs
pApplications = mkPath "/Applications"

-- | Installer packages are XAR archives.
isPkg :: Driver :> es => Path Abs -> Eff es Bool
isPkg file = (== "xar!") <$> drvRunOutputBinary ("head" :| ["-c", "4", file.text])

installPkg :: Driver :> es => Path Abs -> Eff es ()
installPkg pkg = do
  sudo' <- mkSudo
  drvRun $ sudo' $ "installer" :| ["-pkg", pkg.text, "-target", "/"]

withMountedDMG :: Driver :> es => Path Abs -> (Path Abs -> Eff es a) -> Eff es a
withMountedDMG image act = drvTempDir $ \mountPoint ->
  bracket_
    (drvRunSilent $ "hdiutil" :| ["attach", "-nobrowse", "-readonly", "-noautoopen", "-mountpoint", mountPoint.text, image.text])
    (drvRunSilent $ "hdiutil" :| ["detach", mountPoint.text])
    (act mountPoint)

installDMG :: Driver :> es => Path Abs -> Eff es ()
installDMG image = withMountedDMG image $ \volume -> do
  apps <- drvFind volume findOptions{maxDepth = Just 1, names = Just ["*.app"]}
  pkgs <- drvFind volume findOptions{maxDepth = Just 1, names = Just ["*.pkg"]}
  when (null apps && null pkgs) $ terror $ "No applications or packages found in " <> image.text
  for_ apps $ \app -> drvCopy app $ pApplications </> app.basename
  for_ pkgs installPkg

macOSInstallURL :: Driver :> es => Text -> Eff es ()
macOSInstallURL url = drvTempDownload url $ \file -> do
  pkg <- isPkg file
  if pkg then drvTempRename "package.pkg" file installPkg else installDMG file

macOSNotImplemented :: x -> Eff es a
macOSNotImplemented _ = terror "macOS installer only supports installation from URL."

-- | Install disk images and installer packages from URLs.
macOS :: Installer
macOS =
  Installer
    { storeKey = "macos"
    , install_ = macOSNotImplemented
    , installURL = macOSInstallURL
    , upgrade_ = macOSNotImplemented
    , getPackageInfo = macOSNotImplemented
    }
