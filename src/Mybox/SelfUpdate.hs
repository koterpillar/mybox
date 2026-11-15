module Mybox.SelfUpdate where

import Data.Text qualified as Text
import Effectful.Dispatch.Dynamic
import System.Posix.Process (executeFile)

import Mybox.Config.CommandLine
import Mybox.Display
import Mybox.Driver
import Mybox.Package.Archive
import Mybox.Package.Class
import Mybox.Package.Forge
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

myboxBinary :: Text
myboxBinary = "mybox"

mybox :: ForgePackage
mybox =
  (mkForgePackage "koterpillar/mybox")
    { archive = emptyArchiveFields{raw = Right True, binaries = [myboxBinary]}
    }

-- Mybox owns its own update files, so they are not user packages to clean up.
selfUpdate :: (AppDisplay :> es, Concurrent :> es, Driver :> es, Execute :> es, Stores :> es) => CommandLine -> Eff es ()
selfUpdate commandLine = nullTracker $ runInstallQueue $ do
  before <- localVersion mybox
  ensureInstalled mybox
  after <- localVersion mybox
  when (before /= after) $ restart $ withoutSelfUpdate commandLine

restart :: (Driver :> es, Execute :> es) => CommandLine -> Eff es ()
restart commandLine = do
  local <- drvLocal False
  let bin = local </> "bin" </> myboxBinary
  send $ Execute (bin.text :| commandLineArgs commandLine)

data Execute :: Effect where
  Execute :: Args -> Execute m ()

type instance DispatchOf Execute = Dynamic

localExecute :: IOE :> es => Eff (Execute : es) a -> Eff es a
localExecute = do
  interpret_ $ \case
    Execute (program :| args) -> liftIO $ executeFile (Text.unpack program) False (map Text.unpack args) Nothing
