module Mybox.Utils where

import Crypto.Hash (SHA256 (..), hashWith)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath.Glob (compile, match)

glob :: Text -> Text -> Bool
glob pattern text = match (compile $ Text.unpack pattern) (Text.unpack text)

sha256 :: ByteString -> Text
sha256 = Text.pack . show . hashWith SHA256

-- | Check if a repository URL is a GitHub shortcut (e.g., "user/repo").
isGithubShortcut :: Text -> Bool
isGithubShortcut r =
  not (Text.isPrefixOf "https://" r) && not (Text.isInfixOf "@" r)

-- | Normalize GitHub shortcuts to full URLs.
normalizeGitRepoUrl :: Text -> Text
normalizeGitRepoUrl r
  | isGithubShortcut r = "https://github.com/" <> r <> ".git"
  | otherwise = r
