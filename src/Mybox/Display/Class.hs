module Mybox.Display.Class (
  Log,
  Banner,
  ANSIDisplayable,
  Display (..),
  Color (..),
  TerminalItem (..),
  tiMk,
  tiSpace,
  tiComma,
  TerminalShow (..),
  displayLog,
  displaySetBanner,
  displayModifyBanner,
  displayModifyBannerWhile,
) where

import Data.Kind
import Effectful.Dispatch.Dynamic
import System.Console.ANSI
import Prelude hiding (log)

import Mybox.Prelude

data family Log a

data family Banner a

type ANSIDisplayable a = (Monoid (Banner a), TerminalShow (Banner a), TerminalShow (Log a))

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
  , text :: Text
  }

tiMk :: Text -> TerminalItem
tiMk text = TerminalItem{foreground = Nothing, text}

tiComma :: TerminalItem
tiComma = tiMk ","

tiSpace :: TerminalItem
tiSpace = tiMk " "

class TerminalShow a where
  terminalShow :: a -> [[TerminalItem]]
