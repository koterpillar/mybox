module Mybox.Display.Class (
  Display (..),
  Log,
  Banner,
  TerminalItem (..),
  mkTerminalItem,
  TerminalShow (..),
  dumbShow,
  displayLog,
  displaySetBanner,
  displayModifyBanner,
  displayModifyBannerWhile,
) where

import Data.Kind
import Data.String
import Data.Text qualified as Text
import Effectful.Dispatch.Dynamic
import Prelude hiding (log)

import Mybox.Prelude

data family Log a

data family Banner a

data Display :: Type -> Effect where
  Log :: Log a -> Display a m ()
  SetBanner :: Banner a -> Display a m ()
  GetBanner :: Display a m (Banner a)

type instance DispatchOf (Display _) = Dynamic

displayLog :: Display a :> es => Log a -> Eff es ()
displayLog log = send $ Log log

displaySetBanner :: Display a :> es => Banner a -> Eff es ()
displaySetBanner banner = send $ SetBanner banner

displayGetBanner :: Display a :> es => Eff es (Banner a)
displayGetBanner = send GetBanner

displayModifyBanner :: Display a :> es => (Banner a -> Banner a) -> Eff es ()
displayModifyBanner f = displayGetBanner >>= displaySetBanner . f

displayModifyBannerWhile :: Display a :> es => (Banner a -> Banner a) -> Eff es r -> Eff es r
displayModifyBannerWhile f act = do
  banner <- displayGetBanner
  displaySetBanner (f banner)
  act `finally` displaySetBanner banner

data TerminalItem = TerminalItem
  { foreground :: Maybe ()
  , background :: Maybe ()
  , text :: Text
  }

instance IsString TerminalItem where
  fromString = mkTerminalItem . Text.pack

mkTerminalItem :: Text -> TerminalItem
mkTerminalItem text = TerminalItem{foreground = Nothing, background = Nothing, text}

class TerminalShow a where
  terminalShow :: a -> [[TerminalItem]]

dumbShow :: TerminalShow a => a -> String
dumbShow = Text.unpack . Text.unlines . map (Text.unwords . map (.text)) . terminalShow
