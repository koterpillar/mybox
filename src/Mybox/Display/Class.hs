module Mybox.Display.Class (
  Log,
  Banner,
  ANSIDisplayable,
  Display (..),
  Color (..),
  TerminalItem (..),
  TerminalLine,
  tiMk,
  tiSpace,
  tiComma,
  TerminalShow (..),
  displayLog,
  displayBanner,
  displayBannerWhile,
) where

import Data.Kind
import Effectful.Dispatch.Dynamic
import System.Console.ANSI
import Prelude hiding (log)

import Mybox.Prelude

data family Log a

data family Banner a

type ANSIDisplayable a =
  ( Eq (Banner a)
  , Monoid (Banner a)
  , TerminalShow (Banner a)
  , TerminalShow (Log a)
  )

data Display :: Type -> Effect where
  Log :: Log a -> Display a m ()
  AddBanner :: Banner a -> Display a m ()
  RemoveBanner :: Banner a -> Display a m ()

type instance DispatchOf (Display _) = Dynamic

displayLog :: Display a :> es => Log a -> Eff es ()
displayLog log = send $ Log log

displayBanner :: Display a :> es => Banner a -> Eff es ()
displayBanner = send . AddBanner

displayRemoveBanner :: Display a :> es => Banner a -> Eff es ()
displayRemoveBanner = send . RemoveBanner

displayBannerWhile :: Display a :> es => Banner a -> Eff es r -> Eff es r
displayBannerWhile b act = do
  displayBanner b
  act `finally` displayRemoveBanner b

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

type TerminalLine = [TerminalItem]

class TerminalShow a where
  terminalShow :: a -> [TerminalLine]
