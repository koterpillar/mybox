module Mybox.Compute.URL where

import Data.Aeson.Types

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Driver
import Mybox.Prelude

urlProcessor :: Driver :> es => Processor (Eff es)
urlProcessor value _ = do
  url <- parseThrow parseJSON value
  String <$> drvHttpGet url
