module Mybox.Installer.Apt (Apt (..)) where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Prelude

data Apt = Apt

instance Installer Apt where
  iInstall Apt = aptInstall
  iUpgrade Apt = aptInstall
  iInstalledVersion Apt package = do
    result <-
      drvRunOutput_ RunExitReturn $
        "dpkg-query"
          :| [ "--showformat"
             , "${Version}"
             , "--show"
             , package
             ]
    pure $ if result.exit == ExitSuccess then Just result.output else Nothing
  iLatestVersion Apt package = do
    output <-
      drvRunOutput $
        "apt-cache"
          :| [ "show"
             , "--quiet"
             , "--no-all-versions"
             , package
             ]
    maybe (terror $ "Cannot determine version for: " <> package) pure $ parseAptCacheOutput output

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
