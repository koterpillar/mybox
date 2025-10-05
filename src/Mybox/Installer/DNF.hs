module Mybox.Installer.DNF (dnf) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Prelude

dnfInstall :: Driver :> es => Text -> Text -> Eff es ()
dnfInstall action package =
  drvRun $
    sudo $
      "dnf"
        :| [ action
           , "-y"
           , package
           ]

rpmQuery :: Driver :> es => Maybe Text -> Eff es (Map Text Text)
rpmQuery package_ = do
  let args =
        "rpm"
          :| [ "--query"
             , "--queryformat"
             , "%{NAME} %{VERSION}\n"
             ]
          <> case package_ of
            Just package -> ["--whatprovides", package]
            Nothing -> ["--all"]
  result <- drvRunOutputExit args
  pure $ if result.exit == ExitSuccess then parseVersions result.output package_ else mempty

dnfRepoQuery :: Driver :> es => Maybe Text -> Eff es (Map Text Text)
dnfRepoQuery package_ = do
  when (isNothing package_) $
    drvRunSilent $
      "dnf" :| ["--quiet", "clean", "expire-cache"]

  arch <- drvArchitecture
  let archStr = case arch of
        X86_64 -> "x86_64"
        Aarch64 -> "aarch64"

  result <-
    drvRunOutput $
      "dnf"
        :| [ "--quiet"
           , "repoquery"
           , "--queryformat"
           , "%{name} %{version}\n"
           , "--latest-limit"
           , "1"
           , "--arch"
           , archStr <> ",noarch"
           ]
        <> case package_ of
          Just package -> ["--whatprovides", package]
          Nothing -> []

  pure $ parseVersions result package_

parseVersions :: Text -> Maybe Text -> Map Text Text
parseVersions output package_ =
  let versions = Map.fromList $ mapMaybe parseLine (Text.lines output)
      parseLine line = case Text.words line of
        [name, version] -> Just (name, version)
        _ -> Nothing
   in case package_ of
        Nothing -> versions
        Just package ->
          case Map.lookup package versions of
            Just version -> Map.singleton package version
            Nothing ->
              -- If querying for a specific package, the name might be different
              -- because of virtual packages and --whatprovides option.
              -- If there's no package with exact name, require a single match.
              case Map.toList versions of
                [] -> terror $ "No versions for " <> package <> "."
                [(_, version)] -> Map.singleton package version
                _ -> terror $ "Multiple versions for " <> package <> ": " <> Text.pack (show versions) <> "."

dnfPackageInfo :: Driver :> es => Maybe Text -> Eff es (Map Text PackageVersion)
dnfPackageInfo package = iCombineLatestInstalled <$> dnfRepoQuery package <*> rpmQuery package

dnf :: Installer
dnf =
  Installer
    { storeKey = "dnf"
    , install_ = dnfInstall "install"
    , upgrade_ = dnfInstall "upgrade"
    , getPackageInfo = dnfPackageInfo
    }
