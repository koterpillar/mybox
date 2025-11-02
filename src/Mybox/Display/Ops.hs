module Mybox.Display.Ops (
  tiShow,
  tiProgressBar,
  tiComma,
  tiSpace,
  tiSplitAt,
  tiMerge,
  tiWrapLines,
) where

import Data.Char
import Data.List (groupBy)
import Data.Text qualified as Text

import Mybox.Display.Class
import Mybox.Prelude

tiComma :: TerminalItem
tiComma = tiMk ", "

tiSpace :: TerminalItem
tiSpace = tiMk " "

tiShow :: Show a => a -> TerminalItem
tiShow = tiMk . Text.pack . show

tiSplitAt :: Int -> TerminalItem -> (TerminalItem, TerminalItem)
tiSplitAt n t = (t{text = Text.take n (text t)}, t{text = Text.drop n (text t)})

tiMerge :: [TerminalItem] -> [TerminalItem]
tiMerge = map mergeGroup . groupBy sameAttributes
 where
  mergeGroup [] = error "tiMerge: empty group"
  mergeGroup l@(x : _) = x{text = mconcat $ map (.text) l}
  sameAttributes = (==) `on` attributes
  attributes ti = ti{text = ""}

-- | Wrap terminal lines to fit within a specified width
tiWrapLines :: Maybe Int -> [TerminalLine] -> [TerminalLine]
tiWrapLines Nothing = id
tiWrapLines (Just width) = concatMap $ wrapLine width

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

tiProgressBar :: Maybe Int -> Int -> Int -> Maybe TerminalLine
tiProgressBar width progress total
  | total == 0 = Nothing
  | progress >= total = Nothing
  | otherwise = Just [tiMk $ bar <> legend]
 where
  -- How long will the full legend (100/100) be? 100/100 longer than 0/100
  fullLegendLength = 2 + 2 * length (show total)
  legend =
    Text.justifyRight fullLegendLength ' ' $
      " " <> Text.pack (show progress) <> "/" <> Text.pack (show total)
  -- The rest of the space is for the bar
  barWidth = max 10 (fromMaybe 80 width - fullLegendLength)
  bar =
    Text.pack
      [ blockChar $
          progress * barWidth * blockCount `div` total
            - x * blockCount
      | x <- [0 .. barWidth - 1]
      ]

blockCount :: Int
blockCount = 8

-- See Unicode Block Elements: https://unicode.org/charts/nameslist/n_2580.html
blockChar :: Int -> Char
blockChar n
  | n <= 0 = ' '
  | otherwise = chr $ 9616 - min blockCount n
