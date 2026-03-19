module Mybox.Package.BrewRepo.Internal where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Installer.Brew
import Mybox.Package.Class
import Mybox.Package.Post
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Utils

data BrewRepo = BrewRepo
  { name_ :: Text
  , post :: [Text]
  }
  deriving (Eq, Generic, Show)

mkBrewRepo :: Text -> BrewRepo
mkBrewRepo name_ = BrewRepo{name_, post = []}

instance PackageName BrewRepo where
  splitName = genericSplitName' @'["brew"] @'["name_"]

instance FromJSON BrewRepo where
  parseJSON = withObjectTotal "BrewRepo" $ do
    name_ <- takeField "brew_tap"
    post <- takePost
    pure BrewRepo{..}

instance ToJSON BrewRepo where
  toJSON p = object $ ["brew_tap" .= p.name_] <> postToJSON p

tapName :: BrewRepo -> Text
tapName p
  | isGithubShortcut p.name_ = p.name_
  | otherwise = Text.intercalate "/" ["mybox", simplifiedUrl]
 where
  simplifiedUrl =
    p.name_
      & dropProtocol
      & dropUsername
      & dropGitExtension
      & replaceColon
      & replaceSlash
  dropGitExtension t =
    if ".git" `Text.isSuffixOf` t
      then Text.dropEnd 4 t
      else t
  dropProtocol t = case Text.breakOn "://" t of
    (_, "") -> t
    (_, rest) -> Text.drop 3 rest
  dropUsername t = case Text.breakOn "@" t of
    (_, "") -> t
    (_, rest) -> Text.drop 1 rest
  replaceColon = Text.replace ":" "_"
  replaceSlash = Text.replace "/" "_"

brewRepoLocalVersion :: App es => BrewRepo -> Eff es (Maybe Text)
brewRepoLocalVersion p = do
  tappedRepos <- brewRun @SystemPackage drvRunOutput ["tap"]
  pure $
    if tapName p `Text.isInfixOf` tappedRepos
      then Just "installed"
      else Nothing

brewRepoInstall :: App es => BrewRepo -> Eff es ()
brewRepoInstall p =
  brewRun @SystemPackage drvRun ("tap" : args)
 where
  args = case isGithubShortcut p.name_ of
    True -> [p.name_]
    False -> [tapName p, p.name_]

instance Package BrewRepo where
  remoteVersion = const $ pure "installed"
  localVersion = brewRepoLocalVersion
  install = installWithPost brewRepoInstall
