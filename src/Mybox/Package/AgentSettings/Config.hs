module Mybox.Package.AgentSettings.Config where

import Data.Aeson.KeyMap qualified as KM
import Data.List (nub)
import Data.Vector qualified as Vector

import Mybox.Aeson
import Mybox.Prelude

modifyConfig :: [Key] -> (Maybe Value -> Either String Value) -> Value -> Either String Value
modifyConfig [] f value = f $ Just value
modifyConfig (key : rest) f (Object fields) = do
  let existing = KM.lookup key fields
  updated <- case rest of
    [] -> f existing
    _ -> modifyConfig rest f $ fromMaybe (Object mempty) existing
  pure $ Object $ KM.insert key updated fields
modifyConfig _ _ _ = Left "Expected an object in existing settings"

appendItems :: [Value] -> Maybe Value -> Either String Value
appendItems new = \case
  Nothing -> pure $ toJSON $ nub new
  Just (Array existing) -> pure $ toJSON $ nub $ Vector.toList existing <> new
  _ -> Left "Expected a list in existing settings"
