module Mybox.Compute (
  preprocess,
) where

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Driver
import Mybox.Prelude

sigils :: Map Text (Value -> Processor (Eff '[Driver]))
sigils = mempty

preprocess :: forall es. (Driver :> es, IOE :> es) => Processor (Eff es)
preprocess = inject . processSigils sigils
