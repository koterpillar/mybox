module Mybox.Compute.Base where

import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as Map
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Prelude

type Processor m = Value -> m Value

findSigil :: KM.KeyMap a -> Maybe (Text, a, KM.KeyMap a)
findSigil obj =
  let m = KM.toMapText obj
      sigils = filter (Text.isPrefixOf "$" . fst) $ Map.toList m
   in case sigils of
        [(s, v)] -> Just (Text.drop 1 s, v, KM.fromMapText $ Map.delete s m)
        [] -> Nothing
        _ -> error $ "Multiple sigils found: " ++ show (map fst sigils)

processSigils :: Monad m => Map Text (Value -> Processor m) -> Processor m
processSigils sigils (Object obj) =
  case findSigil obj of
    Nothing -> Object <$> traverse (processSigils sigils) obj
    Just (sigil, value, rest) -> case Map.lookup sigil sigils of
      Nothing -> error $ "Unknown sigil: " ++ Text.unpack sigil
      Just processor -> processSigils sigils (Object rest) >>= processor value
processSigils sigils (Array arr) = Array <$> traverse (processSigils sigils) arr
processSigils _ (String s) = pure $ String s
processSigils _ (Number n) = pure $ Number n
processSigils _ (Bool b) = pure $ Bool b
processSigils _ Null = pure Null
