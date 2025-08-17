module Mybox.Main (
  main,
) where

import Data.Text qualified as Text
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
    runDisplay stdout $
      runStores $
        localDriver $ do
          config <- readConfig
          state <- drvMyboxState
          ((), installed) <-
            drvTracker (state </> "files.json") $
              trkSession $
                runInstallQueue $
                  for_ config.packages queueInstall
          displayLogText $ "installed: " <> Text.intercalate ", " (toList installed)
