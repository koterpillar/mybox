module Mybox.Installer.Brew (Brew (..)) where

import Data.Map.Strict qualified as Map

import Mybox.Aeson
import Mybox.Driver
import Mybox.Installer.Class
import Mybox.Prelude
import Mybox.Stores

data Brew = Brew

brewInstall :: Driver :> es => Text -> Text -> Eff es ()
brewInstall action package = do
  -- FIXME: find brew executable
  drvRun $ "brew" :| [action, package]

brewPackageInfo :: Driver :> es => Maybe Text -> Eff es (Map Text PackageVersion)
brewPackageInfo package_ = do
  drvRunSilent $ "brew" :| ["update"]
  info <- drvRunOutput $ "brew" :| ["info", "--json=v2", fromMaybe "--installed" package_]
  parseVersions <$> jsonDecode "brew info" info

data BrewInfo = BrewInfo {formulae :: [BrewFormula], casks :: [BrewCask]}
  deriving (Eq, Generic, Show)

instance FromJSON BrewInfo

data BrewFormula = BrewFormula
  { name :: Text
  , installed :: Maybe Text
  , latest :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON BrewFormula where
  parseJSON = withObject "BrewFormula" $ \o -> do
    name <- o .: "name"
    installedArr <- o .:? "installed"
    installed <-
      fmap join $
        for installedArr $
          withArray "BrewFormula.installed" $
            traverse (withObject "BrewFormula.installed item" (.: "version")) . listToMaybe . toList
    latest <-
      o .: "versions"
        >>= \vo -> do
          version <- vo .: "stable"
          revision <- vo .:? "revision"
          pure $ foldr (\acc v -> acc <> "-" <> v) version revision
    pure BrewFormula{..}

parseFormula :: BrewFormula -> (Text, PackageVersion)
parseFormula formula =
  ( formula.name
  , PackageVersion
      { installed = formula.installed
      , latest = formula.latest
      }
  )

data BrewCask = BrewCask {tap :: Text, token :: Text, installed :: Maybe Text, version :: Text}
  deriving (Eq, Generic, Show)

instance FromJSON BrewCask

parseCask :: BrewCask -> (Text, PackageVersion)
parseCask cask =
  ( if cask.tap == "homebrew/cask" then cask.token else cask.tap <> "/" <> cask.token
  , PackageVersion
      { installed = cask.installed
      , latest = cask.version
      }
  )

parseVersions :: BrewInfo -> Map Text PackageVersion
parseVersions info = Map.fromList $ map parseCask info.casks ++ map parseFormula info.formulae

instance Installer Brew where
  iStorePackages Brew = jsonStore "brew"
  iStoreGlobal Brew = jsonStore "brew-global"
  iInstall_ Brew = brewInstall "install"
  iUpgrade_ Brew = brewInstall "upgrade"
  iGetPackageInfo Brew = brewPackageInfo
