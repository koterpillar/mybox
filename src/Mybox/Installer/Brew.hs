{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Installer.Brew where

import Data.Map.Strict qualified as Map

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Installer.Class
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Prelude

data BrewBootstrap s = BrewBootstrap deriving (Eq, Show)

instance HasField "name" (BrewBootstrap s) Text where
  getField _ = "homebrew"

instance FromJSON (BrewBootstrap s) where
  parseJSON = withObjectTotal "BrewBootstrap" $ pure BrewBootstrap

instance ToJSON (BrewBootstrap s) where
  toJSON BrewBootstrap = object []

homebrewDirectory :: Driver :> es => Eff es (Path Abs)
homebrewDirectory = flip fmap drvOS $ \case
  MacOS -> mkPath "/opt/homebrew"
  Linux _ -> mkPath "/home/linuxbrew/.linuxbrew"

instance IsSystemPackage s => Package (BrewBootstrap s) where
  remoteVersion BrewBootstrap = pure "homebrew"
  localVersion BrewBootstrap = do
    dir <- homebrewDirectory
    exists <- drvIsDir dir
    pure $ if exists then Just "homebrew" else Nothing
  install BrewBootstrap = do
    macOS <- flip fmap drvOS $ \case MacOS -> True; _ -> False
    -- macOS comes with git; mkSystemPackage relies on brew itself so have to skip
    unless macOS $ queueInstall $ mkSystemPackage_ @s "git" []
    drvTempDownload "https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh" $ \installSh -> do
      drvMakeExecutable installSh
      drvRun $ installSh.text :| []

brewRun :: forall s es r. (App es, IsSystemPackage s) => (Args -> Eff es r) -> [Text] -> Eff es r
brewRun act args =
  drvAtomic "brew-run" $ do
    queueInstall $ BrewBootstrap @s
    exe <- flip fmap homebrewDirectory $ \dir -> dir </> "bin" </> "brew"
    act $ exe.text :| args

brewInstall :: forall s es. (App es, IsSystemPackage s) => Text -> Text -> Eff es ()
brewInstall action package = brewRun @s drvRun [action, package]

brewPackageInfo :: forall s es. (App es, IsSystemPackage s) => Maybe Text -> Eff es (Map Text PackageVersion)
brewPackageInfo package_ = do
  when (isNothing package_) $
    brewRun @s drvRunSilent ["update"]
  info <- brewRun @s drvRunOutput ["info", "--json=v2", fromMaybe "--installed" package_]
  parseVersions <$> jsonDecode "brew info" info

data BrewInfo = BrewInfo {formulae :: [BrewFormula], casks :: [BrewCask]}
  deriving (Eq, Generic, Show)

instance FromJSON BrewInfo

data BrewFormula = BrewFormula
  { name :: Text
  , installed :: Maybe Text
  , latest :: Text
  }
  deriving (Eq, Show)

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

brew :: forall s. IsSystemPackage s => Installer
brew =
  Installer
    { storeKey = "brew"
    , install_ = brewInstall @s "install"
    , installURL = iURLNotImplemented
    , upgrade_ = brewInstall @s "upgrade"
    , getPackageInfo = brewPackageInfo @s
    }
