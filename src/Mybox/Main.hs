module Mybox.Main (
  main,
) where

import Mybox.Config
import Mybox.Driver
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

main :: IO ()
main =
  runEff $ do
    runStores $
      localDriver $ do
        config <- getConfig
        state <- drvMyboxState
        drvTracker (state </> "files.json") $
          trkSession $
            runInstallQueue $
              for_ config.packages queueInstall
