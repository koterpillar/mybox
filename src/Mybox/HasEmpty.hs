module Mybox.HasEmpty (HasEmpty (..)) where

import Data.Text (Text)

class Eq a => HasEmpty a where
  emptyValue :: a

instance HasEmpty Text where
  emptyValue = ""

instance Eq a => HasEmpty [a] where
  emptyValue = []

instance Eq a => HasEmpty (Maybe a) where
  emptyValue = Nothing

instance HasEmpty Bool where
  emptyValue = False
