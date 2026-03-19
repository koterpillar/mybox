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
  | otherwise = twoSegments
 where
  twoSegments =
    p.name_
      & dropProtocol
      & dropUsername
      & dropGitExtension
      & replaceColon
      & leaveTwoSegments
  dropGitExtension t = fromMaybe t $ Text.stripSuffix ".git" t
  stripUntil_ prefix t = case Text.breakOn prefix t of
    (_, "") -> t
    (_, rest) -> Text.drop (Text.length prefix) rest
  dropProtocol = stripUntil_ "://"
  dropUsername = stripUntil_ "@"
  replaceColon = Text.replace ":" "/"
  leaveTwoSegments t = Text.intercalate "/" $
    case reverse $ filter (not . Text.null) $ Text.splitOn "/" t of
      [] -> error "empty Brew tap URL"
      [""] -> error "empty Brew tap URL"
      [s] -> [s, "_"]
      (s : ss) -> [Text.intercalate "_" $ reverse ss, s]

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
