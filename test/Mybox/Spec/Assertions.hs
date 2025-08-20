module Mybox.Spec.Assertions where

import Data.Map qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Prelude
import Mybox.SpecBase

assertAnyFileExists ::
  (Driver :> es, IOE :> es) =>
  Text ->
  Path Abs ->
  [Path Rel] ->
  Eff es ()
assertAnyFileExists !name dir' paths = do
  local <- drvLocal
  let dir = local <//> dir'

  drvIsDir dir >>= (`shouldBe` True)

  exists <- anyM (\p -> drvIsFile (dir <//> p)) paths
  unless exists $ do
    allFiles <- drvFind dir (mempty{onlyFiles = True})
    expectationFailure $
      Text.unpack name <> "' not found in '" <> show dir <> "'. Existing files: " <> show allFiles

assertKeyExists :: (IOE :> es, Ord k, Show k) => Text -> k -> Map k v -> Eff es v
assertKeyExists !name k m = do
  case Map.lookup k m of
    Just v -> pure v
    Nothing -> do
      expectationFailure $ "Key '" <> show k <> "' not found in " <> Text.unpack name
      error "Unreachable"
