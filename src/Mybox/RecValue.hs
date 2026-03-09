module Mybox.RecValue (RecValue (..)) where

import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text

class RecValue a where
  rvEmpty :: a
  rvNull :: a -> Bool
  default rvNull :: Eq a => a -> Bool
  rvNull = (== rvEmpty)
  rvText :: a -> Maybe Text
  rvText _ = Nothing

instance RecValue Text where
  rvEmpty = ""
  rvNull = Text.null
  rvText = Just

instance RecValue [a] where
  rvEmpty = []
  rvNull = null

instance RecValue (Maybe a) where
  rvEmpty = Nothing
  rvNull = isNothing

instance RecValue Bool where
  rvEmpty = False
  rvNull = not
  rvText True = Just "true"
  rvText False = Just ""
