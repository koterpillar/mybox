module Mybox.SpecHook where

import Control.Concurrent.MVar
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Ord (Down (..))
import Data.Text qualified as Text
import Test.Hspec qualified as Hspec

import Mybox.Display.None
import Mybox.Driver
import Mybox.Driver.Stats
import Mybox.Driver.Test
import Mybox.Prelude hiding (MVar, modifyMVar_, newMVar, takeMVar)
import Mybox.SpecBase
import Mybox.Stores

hook :: Spec -> Hspec.Spec
hook spec = do
  stores <- runIO $ newMVar Map.empty
  afterAll_ (runEff $ runConcurrent $ runStoresWith stores $ printStats 20) $
    parallel $
      effSpec (dispatch stores) spec

dispatch :: MVar MegaStore -> Eff BaseEff r -> Eff '[IOE] r
dispatch stores act =
  runConcurrent $
    noDisplay $
      runStoresWith stores $
        testDriver $
          driverStats act

printStats :: (Concurrent :> es, IOE :> es, Stores :> es) => Int -> Eff es ()
printStats n = do
  stats <- storeGet driverStatsStore
  let top = take n $ sortOn (Down . snd) $ Map.toList stats
  liftIO $ putStrLn $ "Top " <> show n <> " commands:"
  forM_ top $ \(arg, count) ->
    liftIO $ putStrLn $ Text.unpack (shellJoin arg) <> ": " <> show count
