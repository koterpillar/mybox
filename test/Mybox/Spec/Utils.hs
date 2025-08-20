module Mybox.Spec.Utils where

import Mybox.Prelude

requireJust :: String -> Maybe a -> a
requireJust !msg = fromMaybe (error msg)
