module Mybox.Main (
  main,
) where

import Effectful.Concurrent
import System.IO (stdout)

import Mybox.Config
import Mybox.Display
import Mybox.Driver
import Mybox.JSONCodec
import Mybox.Package.Queue
import Mybox.Package.Some
import Mybox.Prelude
import Mybox.SelfUpdate qualified as SelfUpdate
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
mainOpt CLPackageHelp = displayDoc (jsonCodec @SomePackage).doc
mainOpt commandLine =
  runStores $
    localDriver $ do
      when commandLine.selfUpdate $ SelfUpdate.localExecute $ SelfUpdate.selfUpdate commandLine
      state <- drvMyboxState
      drvTracker (state </> "files.json") $
        runInstallQueue $
          do
            config <- readConfig commandLine
            queueInstallMany config.packages
