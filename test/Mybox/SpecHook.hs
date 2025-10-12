module Mybox.SpecHook where

import Data.Map qualified as Map
import Test.Hspec qualified as Hspec

import Mybox.Display.None
import Mybox.Driver.Stats
import Mybox.Driver.Test
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores

hook :: Spec -> Hspec.Spec
hook spec = do
  statVar <- runIO $ runEff $ runConcurrent $ newMVar Map.empty
  driverLock <- runIO $ runEff $ runConcurrent $ newMVar ()
  afterAll_ (runEff $ runConcurrent $ printStats statVar 20) $
    effSpec (dispatch statVar driverLock) spec

dispatch :: MVar DriverStats -> MVar () -> Eff BaseEff r -> Eff '[IOE] r
dispatch statVar driverLock act =
  runConcurrent $
    noDisplay $
      runStores $
        testDriver driverLock $
          driverStats statVar act
