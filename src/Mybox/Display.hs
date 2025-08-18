module Mybox.Display (
  runDisplay,
  noDisplay,
  displayLog,
  displayModifyBanner,
  displayModifyBannerWhile,
  AppDisplay,
  addInstalling,
  MDisplay,
  displayLogText,
) where

import Mybox.Display.Class
import Mybox.Display.Data
import Mybox.Display.None
import Mybox.Display.Simple
import Mybox.Prelude

type AppDisplay = Display MDisplay

runDisplay :: IOE :> es => Eff (AppDisplay : es) r -> Eff es r
runDisplay = runSimpleDisplay

displayLogText :: AppDisplay :> es => Text -> Eff es ()
displayLogText = displayLog . MLog
