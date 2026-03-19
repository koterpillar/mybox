module Mybox.Utils where

import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath.Glob (compile, match)

glob :: Text -> Text -> Bool
glob pattern text = match (compile $ Text.unpack pattern) (Text.unpack text)

-- | Check if a repository URL is a GitHub shortcut (e.g., "user/repo").
isGithubShortcut :: Text -> Bool
isGithubShortcut r =
  not (Text.isPrefixOf "https://" r) && not (Text.isInfixOf "@" r)

-- | Normalize GitHub shortcuts to full URLs.
normalizeGitRepoUrl :: Text -> Text
normalizeGitRepoUrl r
  | isGithubShortcut r = "https://github.com/" <> r <> ".git"
  | otherwise = r
