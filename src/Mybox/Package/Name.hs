module Mybox.Package.Name where

import Data.Text qualified as Text
import GHC.Generics

import Mybox.Prelude

class HasField "name" a Text => PackageName a where
  -- | Return the same package value with an empty 'name' field.
  --
  -- Returns 'Nothing' when the package is fully determined by its name.
  withoutName :: a -> Maybe a

pathname :: PackageName p => p -> Text
pathname p = Text.replace "/" "--" p.name

genericWithoutName :: (GWithoutName (Rep a), Generic a) => a -> Maybe a
genericWithoutName = genericWithoutName' ["name"]

genericWithoutName' :: (GWithoutName (Rep a), Generic a) => [String] -> a -> Maybe a
genericWithoutName' nameFields value =
  let repWithoutName = gWithoutName nameFields $ from value
      valueWithoutName = to repWithoutName
   in if allDefault nameFields repWithoutName
        then Nothing
        else Just valueWithoutName

class GWithoutName f where
  gWithoutName :: [String] -> f p -> f p
  allDefault :: [String] -> f p -> Bool

-- No instance for U1 since there must be at least one field for the package name

instance (GWithoutName left, GWithoutName right) => GWithoutName (left :*: right) where
  gWithoutName nameFields (left :*: right) =
    gWithoutName nameFields left :*: gWithoutName nameFields right
  allDefault nameFields (left :*: right) =
    allDefault nameFields left && allDefault nameFields right

-- No instance for sums (:+:) since there is only a single default

instance GWithoutName inner => GWithoutName (M1 D meta inner) where
  gWithoutName nameFields (M1 inner) = M1 $ gWithoutName nameFields inner
  allDefault nameFields (M1 inner) = allDefault nameFields inner

instance GWithoutName inner => GWithoutName (M1 C meta inner) where
  gWithoutName nameFields (M1 inner) = M1 $ gWithoutName nameFields inner
  allDefault nameFields (M1 inner) = allDefault nameFields inner

instance (HasEmpty value, Selector selector) => GWithoutName (M1 S selector (K1 index value)) where
  gWithoutName nameFields field@(M1 (K1 value))
    | selName field `elem` nameFields = M1 $ K1 emptyValue
    | otherwise = M1 $ K1 value
  allDefault nameFields field@(M1 (K1 value))
    | selName field `elem` nameFields = True
    | otherwise = value == emptyValue
