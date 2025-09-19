module Mybox.Main (
  main,
) where

import Effectful.Concurrent

import Mybox.Config
import Mybox.Driver
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

main :: IO ()
main =
  runEff $
    runConcurrent $
      runStores $
        localDriver $ do
          config <- readConfig
          state <- drvMyboxState
          drvTracker (state </> "files.json") $
            runInstallQueue_ $
              for_ config.packages queueInstall
