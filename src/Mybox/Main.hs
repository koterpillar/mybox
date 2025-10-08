module Mybox.Main (
  main,
) where

import Data.Text qualified as Text
import Data.Version
import Effectful.Concurrent
import System.IO (stdout)

import Mybox.Config
import Mybox.Display
import Mybox.Driver
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker
import Paths_mybox (version)

main :: IO ()
main =
  runEff $
    runConcurrent $
      runDisplay stdout $ do
        displayLogText $ "mybox " <> Text.pack (showVersion version)
        runStores $
          localDriver $ do
            config <- readConfig
            state <- drvMyboxState
            drvTracker (state </> "files.json") $
              runInstallQueue $
                queueInstallMany config.packages
