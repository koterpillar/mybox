module Mybox.Driver.Stats (driverStats) where

import Effectful.Dispatch.Dynamic

import Mybox.Driver.Class
import Mybox.Prelude
import Mybox.Stores

statsStore :: Store Args Int
statsStore = mkStore "driverStats" jsonIso jsonIso

driverStats :: (Driver :> es, Stores :> es) => Eff es a -> Eff es (Map Args Int, a)
driverStats act = do
  storeClear statsStore
  r <- interposeWith_ act $ \case
    DrvRun e o args -> do
      storeAdjust statsStore args $ Just . succ . fromMaybe 0
      send $ DrvRun e o args
  stats <- storeGetAll statsStore
  pure (stats, r)
