module Mybox.Main
  ( main
  ) where

import           Mybox.Driver
import           Mybox.Prelude

import           System.Exit

main :: IO ()
main =
  runEff
    $ localDriver
    $ do
        drvRun $ "echo" :| ["Not implemented yet."]
        liftIO exitFailure
