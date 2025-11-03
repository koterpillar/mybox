module Mybox.Compute.Base where

import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as Map
import Data.Text qualified as Text
import Witherable (wither)

import Mybox.Aeson
import Mybox.Prelude

type Processor m = Value -> Object -> m (Maybe Value)

findSigil :: KM.KeyMap a -> Maybe (Text, a, KM.KeyMap a)
findSigil obj =
  let m = KM.toMapText obj
      sigils = filter (Text.isPrefixOf "$" . fst) $ Map.toList m
   in case sigils of
        [(s, v)] -> Just (Text.drop 1 s, v, KM.fromMapText $ Map.delete s m)
        [] -> Nothing
        _ -> terror $ "Multiple sigils found: " <> Text.intercalate ", " (map fst sigils) <> " in " <> Text.pack (show $ KM.keys obj)

processSigils :: Monad m => Map Text (Processor m) -> Value -> m Value
processSigils sigils = fmap (fromMaybe Null) . processSigils_ sigils

processSigils_ :: Monad m => Map Text (Processor m) -> Value -> m (Maybe Value)
processSigils_ sigils (Object obj) =
  case findSigil obj of
    Nothing -> Just . Object <$> wither (processSigils_ sigils) obj
    Just (sigil, value, rest) ->
      case Map.lookup sigil sigils of
        Nothing -> error $ "Unknown sigil: " ++ Text.unpack sigil
        Just processor ->
          wither (processSigils_ sigils) rest >>= processor value
processSigils_ sigils (Array arr) =
  Just . Array <$> wither (processSigils_ sigils) arr
processSigils_ _ (String s) = pure $ Just $ String s
processSigils_ _ (Number n) = pure $ Just $ Number n
processSigils_ _ (Bool b) = pure $ Just $ Bool b
processSigils_ _ Null = pure $ Just Null
