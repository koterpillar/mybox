module Mybox.Display.Ops (
  tiShow,
  tiProgressBar,
  tiComma,
  tiSpace,
  tiSplitAt,
  tiMerge,
  tiWrapLines,
) where

import Data.List (groupBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text

import Mybox.Display.Class
import Mybox.Prelude

tiComma :: TerminalItem
tiComma = tiMk ","

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
  | otherwise = Just [tiMk $ filledPart <> emptyPart, tiSpace, tiShow progress, tiMk "/", tiShow total]
 where
  -- Reserve space for the numbers
  availableWidth = max 10 (fromMaybe 80 width - 10)
  filledChars = (progress * availableWidth) `div` total
  emptyChars = availableWidth - filledChars
  filledPart = Text.replicate filledChars "#"
  emptyPart = Text.replicate emptyChars " "
