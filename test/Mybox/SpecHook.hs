module Mybox.SpecHook where

import Data.Map qualified as Map
import System.Environment (unsetEnv)
import Test.Hspec qualified as Hspec

import Mybox.Display.None
import Mybox.Driver
import Mybox.Driver.Stats
import Mybox.Driver.Test
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores

hook :: Spec -> Hspec.Spec
hook spec = do
  -- pipx tests might run in parallel; pipx needs to use the default location
  -- for virtual environments (inside $HOME) to avoid conflicts.
  runIO $ unsetEnv "PIPX_HOME"
  runIO $ unsetEnv "PIPX_BIN_DIR"
  statVar <- runIO $ runEff $ runConcurrent $ newMVar Map.empty
  driverLock <- runIO $ runEff $ runConcurrent $ newLockMap
  afterAll_ (runEff $ runConcurrent $ printStats statVar 20) $
    parallel $
      effSpec (dispatch statVar driverLock) spec

dispatch :: MVar DriverStats -> DriverLockMap -> Eff BaseEff r -> Eff '[IOE] r
dispatch statVar driverLock act =
  runConcurrent $
    noDisplay $
      runStores $
        testDriver driverLock $
          driverStats statVar act
