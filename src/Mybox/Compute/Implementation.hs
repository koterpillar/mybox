module Mybox.Compute.Implementation where

import Data.Aeson.KeyMap qualified as KM
import Witherable qualified as W

import Mybox.Aeson

filterImplementation :: Value -> Value
filterImplementation (Array arr) = Array $ W.mapMaybe hasImpl arr
 where
  hasImpl (Object obj)
    | KM.lookup impl obj == Just (String "haskell") = Just $ Object $ KM.delete impl obj
    | otherwise = Nothing
  hasImpl v = Just v
  impl = "$implementation"
filterImplementation v = v
