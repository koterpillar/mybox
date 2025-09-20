module Mybox.Spec.Uncovered.Utils where

import Mybox.Prelude

requireJust :: String -> Maybe a -> a
requireJust !msg = fromMaybe (error msg)

requireRight :: Show e => Either e a -> a
requireRight = either (error . show) id
