module Mybox.Display.SpecUtils where

import Data.Text qualified as Text
import System.IO

import Mybox.Driver
import Mybox.Prelude

writeHandle :: (Concurrent :> es, IOE :> es) => (Handle -> Eff es r) -> Eff es (r, String)
writeHandle act = localDriver $ drvTempFile $ \filePath -> do
  let fileName = Text.unpack filePath.text
  r <-
    bracket
      (liftIO $ openFile fileName WriteMode)
      (liftIO . hClose)
      (inject . act)
  contents <- liftIO $ readFile fileName
  pure (r, contents)
