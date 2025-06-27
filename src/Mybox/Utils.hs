module Mybox.Utils where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Prelude

repoBranchVersion ::
  Driver :> es =>
  -- | Repository
  Text ->
  -- | Branch
  Maybe Text ->
  Eff es Text
repoBranchVersion repo_ branch_ = do
  let repo = fromMaybe repo_ $ Text.stripPrefix "git+" repo_
  let branch = fromMaybe "HEAD" branch_
  output <- drvRunOutput $ "git" :| ["ls-remote", repo, branch]
  case Text.words output of
    [ref, _] -> pure ref
    _ -> error $ "Failed to parse git ls-remote output: " <> Text.unpack output
