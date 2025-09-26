module Mybox.Compute (
  preprocess,
) where

import Data.Map qualified as Map

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Compute.Condition
import Mybox.Compute.Format
import Mybox.Compute.Implementation
import Mybox.Compute.JSONPath
import Mybox.Compute.Links
import Mybox.Compute.URL
import Mybox.Driver
import Mybox.Prelude

sigils :: Map Text (Processor (Eff '[Driver]))
sigils =
  Map.fromList
    [ ("if", conditionProcessor)
    , ("format", formatProcessor)
    , ("jsonpath", jsonpathProcessor)
    , ("links", linksProcessor)
    , ("url", urlProcessor)
    ]

preprocess :: Driver :> es => Value -> Eff es Value
preprocess = inject . processSigils sigils . filterImplementation
