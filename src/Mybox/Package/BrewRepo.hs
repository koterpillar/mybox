module Mybox.Package.BrewRepo (BrewRepo (..), mkBrewRepo) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Installer.Brew
import Mybox.Package.Class
import Mybox.Package.Post
import Mybox.Package.System
import Mybox.Prelude

data BrewRepo = BrewRepo
  { name_ :: Text
  , post :: [Text]
  }
  deriving (Eq, Show)

mkBrewRepo :: Text -> BrewRepo
mkBrewRepo name_ = BrewRepo{name_, post = []}

instance HasField "name" BrewRepo Text where
  getField p = "brew-" <> p.name_

instance FromJSON BrewRepo where
  parseJSON = withObjectTotal "BrewRepo" $ do
    name_ <- takeField "brew_tap"
    post <- takePost
    pure BrewRepo{..}

instance ToJSON BrewRepo where
  toJSON p = object $ ["brew_tap" .= p.name_] <> postToJSON p

brewRepoLocalVersion :: App es => BrewRepo -> Eff es (Maybe Text)
brewRepoLocalVersion p = do
  tappedRepos <- brewRun @SystemPackage drvRunOutput ["tap"]
  pure $
    if p.name_ `Text.isInfixOf` tappedRepos
      then Just "installed"
      else Nothing

brewRepoInstall :: App es => BrewRepo -> Eff es ()
brewRepoInstall p = brewRun @SystemPackage drvRun ["tap", p.name_]

instance Package BrewRepo where
  remoteVersion = const $ pure "installed"
  localVersion = brewRepoLocalVersion
  install = installWithPost brewRepoInstall
