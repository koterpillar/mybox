module Mybox.Display.SpecUtils where

import Data.Text qualified as Text
import System.IO

import Mybox.Display.Class
import Mybox.Display.Print (Print)
import Mybox.Display.Print qualified as Print
import Mybox.Display.Simple
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

runSimpleDisplayPure :: forall a es r. ANSIDisplayable a => Eff (Display a : es) r -> Eff es (r, String)
runSimpleDisplayPure act =
  Print.runPure $
    runSimpleDisplay $
      inject @_ @(Display a : Print : _) act
