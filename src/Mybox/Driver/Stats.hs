module Mybox.Driver.Stats (DriverStats, driverStats, printStats) where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text qualified as Text
import Effectful.Dispatch.Dynamic

import Mybox.Driver.Class
import Mybox.Driver.Ops
import Mybox.Prelude

type DriverStats = Map Args Int

driverStats :: (Concurrent :> es, Driver :> es) => MVar DriverStats -> Eff es a -> Eff es a
driverStats statVar =
  interpose_ $ \case
    DrvRun e o args -> do
      modifyMVarPure statVar $ Map.insertWith (+) args 1
      send $ DrvRun e o args

printStats :: (Concurrent :> es, IOE :> es) => MVar DriverStats -> Int -> Eff es ()
printStats statVar n = do
  stats <- takeMVar statVar
  let top = take n $ sortOn (Down . snd) $ Map.toList stats
  liftIO $ putStrLn $ "Top " <> show n <> " commands:"
  forM_ top $ \(arg, count) ->
    liftIO $ putStrLn $ Text.unpack (shellJoin arg) <> ": " <> show count
