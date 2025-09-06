module Mybox.Display (
  runDisplay,
  displayLog,
  displayModifyBanner,
  displayModifyBannerWhile,
  AppDisplay,
  addChecking,
  addInstalling,
  MDisplay,
  displayLogText,
) where

import System.IO (Handle)

import Mybox.Display.ANSI
import Mybox.Display.Class
import Mybox.Display.Data
import Mybox.Display.Print (Print)
import Mybox.Display.Print qualified as Print
import Mybox.Display.Simple
import Mybox.Prelude

type AppDisplay = Display MDisplay

runDisplay :: IOE :> es => Handle -> Eff (AppDisplay : es) r -> Eff es r
runDisplay h act = do
  isANSI <- supportsANSI h
  let run = if isANSI then runANSIDisplay else runSimpleDisplay
  Print.run h $ run $ inject @_ @(AppDisplay : Print : _) act

displayLogText :: AppDisplay :> es => Text -> Eff es ()
displayLogText = displayLog . MLog
