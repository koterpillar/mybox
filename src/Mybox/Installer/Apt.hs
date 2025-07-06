module Mybox.Installer.Apt (Apt (..)) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Prelude
import Mybox.Stores

data Apt = Apt

aptInstall :: Driver :> es => Text -> Eff es ()
aptInstall package =
  drvRun $
    "sudo"
      :| [ "env"
         , "DEBIAN_FRONTEND=noninteractive"
         , "apt"
         , "install"
         , "-y"
         , package
         ]

parseAptCacheOutput :: Text -> Maybe Text
parseAptCacheOutput output = listToMaybe $ do
  line <- Text.lines output
  toList $ Text.stripPrefix "Version: " line

aptInstalled :: Driver :> es => Text -> Eff es (Maybe Text)
aptInstalled package = do
  result <-
    drvRunOutputExit $
      "dpkg-query"
        :| [ "--showformat"
           , "${Version}"
           , "--show"
           , package
           ]
  pure $ if result.exit == ExitSuccess then Just result.output else Nothing

aptLatest :: Driver :> es => Text -> Eff es Text
aptLatest package = do
  output <-
    drvRunOutput $
      "apt-cache"
        :| [ "show"
           , "--quiet"
           , "--no-all-versions"
           , package
           ]
  maybe (terror $ "Cannot determine version for: " <> package) pure $ parseAptCacheOutput output

aptPackageInfo :: Driver :> es => Maybe Text -> Eff es (Map Text PackageVersion)
aptPackageInfo Nothing = pure mempty
aptPackageInfo (Just package) = fmap (Map.singleton package) $ PackageVersion <$> aptInstalled package <*> aptLatest package

instance Installer Apt where
  iStorePackages Apt = jsonStore "apt"
  iStoreGlobal Apt = jsonStore "apt"
  iInstall_ Apt = aptInstall
  iUpgrade_ Apt = aptInstall
  iGetPackageInfo Apt = aptPackageInfo
