module Mybox.Package.BrewRepo (BrewRepo (..), mkBrewRepo) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Post
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

brewRepoLocalVersion :: Driver :> es => BrewRepo -> Eff es (Maybe Text)
brewRepoLocalVersion p = do
  os <- drvOS
  case os of
    MacOS -> do
      tappedRepos <- drvRunOutput $ "brew" :| ["tap"]
      pure $
        if p.name_ `Text.isInfixOf` tappedRepos
          then Just "installed"
          else Nothing
    Linux _ -> pure Nothing

brewRepoInstall :: Driver :> es => BrewRepo -> Eff es ()
brewRepoInstall p = do
  os <- drvOS
  case os of
    MacOS -> do
      drvRun $ "brew" :| ["tap", p.name_]
    Linux _ -> terror "BrewRepo is only supported on macOS" -- FIXME: technically also supported

instance Package BrewRepo where
  remoteVersion = const $ pure "installed"
  localVersion = brewRepoLocalVersion
  install = installWithPost brewRepoInstall
