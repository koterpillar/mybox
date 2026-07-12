module Mybox.Compute.Facts where

import Mybox.Driver
import Mybox.Prelude
import Mybox.Utils

architectureMatches :: Driver :> es => Text -> Eff es Bool
architectureMatches aStr = case parseArchitecture aStr of
  Left _ -> pure False
  Right a -> (a ==) <$> drvArchitecture

hostnameMatches :: Driver :> es => Text -> Eff es Bool
hostnameMatches h = glob h <$> drvHostname

osMatch :: Driver :> es => Text -> Eff es Bool
osMatch o = osMatches o <$> drvOS
