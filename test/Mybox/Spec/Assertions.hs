module Mybox.Spec.Assertions where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Prelude
import Mybox.SpecBase

assertAnyExists ::
  (Driver :> es, IOE :> es) =>
  Text ->
  Path Abs ->
  [Path Rel] ->
  Eff es ()
assertAnyExists name dir' paths = do
  local <- drvLocal
  let dir = local <//> dir'

  drvIsDir dir >>= (`shouldBe` True)

  exists <- anyM (\p -> drvIsFile (dir <//> p)) paths
  unless exists $ do
    allFiles <- drvFind dir (mempty{onlyFiles = True})
    expectationFailure $
      Text.unpack name <> "' not found in '" <> show dir <> "'. Existing files: " <> show allFiles
