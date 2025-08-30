module Mybox.Display.Class (
  Display (..),
  Log,
  Banner,
  Color (..),
  TerminalItem (..),
  tiMk,
  tiSpace,
  tiComma,
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
import System.Console.ANSI
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
  { foreground :: Maybe Color
  , background :: Maybe Color
  , text :: Text
  }

instance IsString TerminalItem where
  fromString = tiMk . Text.pack

tiMk :: Text -> TerminalItem
tiMk text = TerminalItem{foreground = Nothing, background = Nothing, text}

tiComma :: TerminalItem
tiComma = tiMk ","

tiSpace :: TerminalItem
tiSpace = tiMk " "

class TerminalShow a where
  terminalShow :: a -> [[TerminalItem]]

dumbShow :: TerminalShow a => a -> String
dumbShow = Text.unpack . Text.unlines . map (Text.concat . map (.text)) . terminalShow
