module Mybox.Display.Class (
  Log,
  Banner,
  ANSIDisplayable,
  Display (..),
  Color (..),
  TerminalItem (..),
  TerminalLine,
  tiMk,
  tiSplitAt,
  tiSpace,
  tiComma,
  TerminalShow (..),
  displayLog,
  displayBanner,
  displayBannerWhile,
  wrapLines,
) where

import Data.Kind
import Data.Text qualified as Text
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

tiSplitAt :: Int -> TerminalItem -> (TerminalItem, TerminalItem)
tiSplitAt n t = (t{text = Text.take n (text t)}, t{text = Text.drop n (text t)})

tiComma :: TerminalItem
tiComma = tiMk ","

tiSpace :: TerminalItem
tiSpace = tiMk " "

type TerminalLine = [TerminalItem]

class TerminalShow a where
  terminalShow :: Maybe Int -> a -> [TerminalLine]

instance TerminalShow () where
  terminalShow _ () = []

wrapLine :: Int -> TerminalLine -> [TerminalLine]
wrapLine maxWidth = fillLines []
 where
  fillLines acc [] = reverse acc
  fillLines acc items = let (line, rest) = go 0 [] items in fillLines (line : acc) rest
  go _ acc [] = (reverse acc, [])
  go w acc (x : xs)
    | lw x > maxWidth =
        let (x1, x2) = tiSplitAt (pred maxWidth - w) x
         in (reverse (x1 : acc), (x2 : xs)) -- single too long item
    | w + lw x > maxWidth = (reverse acc, x : xs) -- current item doesn't fit
    | otherwise = go (w + lw x) (x : acc) xs
  lw item = Text.length item.text

wrapLines :: Maybe Int -> [TerminalLine] -> [TerminalLine]
wrapLines Nothing = id
wrapLines (Just width) = concatMap $ wrapLine width
