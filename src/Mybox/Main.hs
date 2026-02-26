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
import Paths_mybox (version)

main :: IO ()
main = runEff $
  runConcurrent $
    runDisplay stdout $
      do
        options <- parseArgs
        mainOpt options

mainOpt :: (AppDisplay :> es, Concurrent :> es, IOE :> es) => CommandLine -> Eff es ()
mainOpt CLVersion = displayVersion version
mainOpt commandLine =
  runStores $
    localDriver $ do
      config <- readConfig commandLine
      state <- drvMyboxState
      drvTracker (state </> "files.json") $
        runInstallQueue $
          queueInstallMany config.packages
