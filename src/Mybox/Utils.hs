module Mybox.Utils where

import qualified Data.Text     as Text

import           Mybox.Driver
import           Mybox.Prelude

repoBranchVersion ::
     MonadDriver m
  => Text -- ^ Repository
  -> Maybe Text -- ^ Branch
  -> m Text
repoBranchVersion repo_ branch_ = do
  let repo = fromMaybe repo_ $ Text.stripPrefix "git+" repo_
  let branch = fromMaybe "HEAD" branch_
  output <- drvRunOutput $ "git" :| ["ls-remote", repo, branch]
  case Text.words output of
    [ref, _] -> pure ref
    _ -> error $ "Failed to parse git ls-remote output: " <> Text.unpack output
