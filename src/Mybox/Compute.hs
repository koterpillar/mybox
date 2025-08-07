module Mybox.Compute (
  preprocess,
) where

import Data.Map qualified as Map

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Compute.Format
import Mybox.Compute.Links
import Mybox.Compute.URL
import Mybox.Driver
import Mybox.Prelude

sigils :: Map Text (Processor (Eff '[Driver]))
sigils =
  Map.fromList
    [ ("url", urlProcessor)
    , ("format", formatProcessor)
    , ("links", linksProcessor)
    ]

preprocess :: (Driver :> es, IOE :> es) => Value -> Eff es Value
preprocess = inject . processSigils sigils
