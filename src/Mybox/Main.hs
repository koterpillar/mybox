module Mybox.Main (
  main,
) where

import System.Exit

import Mybox.Driver
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

main :: IO ()
main =
  runEff $
    runStores $
      localDriver $
        drvTracker (pMyboxState </> "files.json") $
          do
            drvRun $ "echo" :| ["Not implemented yet."]
            liftIO exitFailure
