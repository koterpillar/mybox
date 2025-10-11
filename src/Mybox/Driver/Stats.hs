module Mybox.Driver.Stats (driverStats, driverStatsStore) where

import Data.Map.Strict qualified as Map
import Effectful.Dispatch.Dynamic

import Mybox.Driver.Class
import Mybox.Prelude
import Mybox.Stores

driverStatsStore :: Store (Map [Text] Int)
driverStatsStore = Store{key = "driver-args-stats", iso = jsonIso, def = Map.empty}

driverStats :: (Concurrent :> es, Driver :> es, Stores :> es) => Eff es a -> Eff es a
driverStats =
  interpose_ $ \case
    DrvRun e o args -> do
      storeModify driverStatsStore $ Map.insertWith (+) (toList args) 1
      send $ DrvRun e o args
