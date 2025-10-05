module Mybox.Driver.Stats (driverStats) where

import Data.Map.Strict qualified as Map
import Effectful.Dispatch.Dynamic

import Mybox.Driver.Class
import Mybox.Prelude

driverStats :: (Concurrent :> es, Driver :> es) => Eff es a -> Eff es (Map Args Int, a)
driverStats act = do
  statVar <- newMVar (Map.empty :: Map Args Int)
  r <- interposeWith_ act $ \case
    DrvRun e o args -> do
      modifyMVarPure statVar $ Map.insertWith (+) args 1
      send $ DrvRun e o args
  stats <- readMVar statVar
  pure (stats, r)
