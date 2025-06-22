module Mybox.Main
  ( main
  ) where

import           Mybox.Driver
import           Mybox.Prelude
import           Mybox.Tracker

import           System.Exit

main :: IO ()
main =
  runEff
    $ localDriver
    $ drvTracker ".local/share/mybox/files.json"
    $ do
        drvRun $ "echo" :| ["Not implemented yet."]
        liftIO exitFailure
