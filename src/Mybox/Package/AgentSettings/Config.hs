module Mybox.Package.AgentSettings.Config where

import Data.List (nub)
import Lens.Micro qualified as Lens
import Lens.Micro.Aeson qualified as AesonLens

import Mybox.Aeson
import Mybox.Prelude

modifyConfig :: [Key] -> (Maybe Value -> Either String Value) -> Value -> Either String Value
modifyConfig [] f value = f $ Just value
modifyConfig (key : rest) f value@(Object _) =
  Lens.traverseOf (AesonLens._Object . Lens.at key) update value
 where
  update existing =
    Just <$> case rest of
      [] -> f existing
      _ -> modifyConfig rest f $ fromMaybe (Object mempty) existing
modifyConfig _ _ _ = Left "Expected an object in existing settings"

appendItems :: [Value] -> Maybe Value -> Either String Value
appendItems new = \case
  Nothing -> pure $ toJSON $ nub new
  Just existing@(Array _) -> pure $ toJSON $ nub $ (existing Lens.^.. AesonLens.values) <> new
  _ -> Left "Expected a list in existing settings"
