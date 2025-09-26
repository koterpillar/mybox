module Mybox.Main (
  main,
) where

import Effectful.Concurrent
import System.IO (stdout)

import Mybox.Config
import Mybox.Display
import Mybox.Driver
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

main :: IO ()
main =
  runEff $
    runConcurrent $
      runDisplay stdout $
        runStores $
          localDriver $ do
            config <- readConfig
            state <- drvMyboxState
            drvTracker (state </> "files.json") $
              runInstallQueue $
                for_ config.packages queueInstall
