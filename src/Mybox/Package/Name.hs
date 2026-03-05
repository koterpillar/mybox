module Mybox.Package.Name where

import Data.Text qualified as Text
import GHC.Generics
import GHC.TypeLits

import Mybox.Prelude

class PackageName a where
  -- | Name lens. Returns the name and the rest of the package value (if any).
  --
  -- If the package is fully determined by its name, the second element of the
  -- tuple will be 'Nothing'.
  splitName :: a -> (Text, Maybe a)

getName :: PackageName a => a -> Text
getName = fst . splitName

withoutName :: PackageName a => a -> Maybe a
withoutName = snd . splitName

pathname :: PackageName p => p -> Text
pathname p = Text.replace "/" "--" (getName p)

joinName :: Maybe Text -> [Text] -> Text
joinName prefix parts = Text.intercalate "#" $ maybe id (:) prefix parts

genericSplitName :: (GHasName '["name"] (Rep a), Generic a) => a -> (Text, Maybe a)
genericSplitName = genericSplitName' Nothing (Proxy @'["name"])

genericSplitName' :: (GHasName names (Rep a), Generic a) => Maybe Text -> proxy names -> a -> (Text, Maybe a)
genericSplitName' prefix names value =
  let r = gSplitName names $ from value
      name = joinName prefix r.parts
   in if r.allDefault
        then (name, Nothing)
        else (name, Just $ to r.rest)

data NameParts p = NameParts {parts :: [Text], rest :: p, allDefault :: Bool}
  deriving (Functor)

liftNameParts :: (a -> b -> c) -> NameParts a -> NameParts b -> NameParts c
liftNameParts f (NameParts partsL restL allDefL) (NameParts partsR restR allDefR) =
  NameParts (partsL <> partsR) (f restL restR) (allDefL && allDefR)

class GHasName (names :: [Symbol]) f where
  gSplitName :: proxy names -> f p -> NameParts (f p)

-- No instance for U1 since there must be at least one field for the package name

instance (GHasName names left, GHasName names right) => GHasName names (left :*: right) where
  gSplitName names (left :*: right) =
    let namePartsL = gSplitName names left
        namePartsR = gSplitName names right
     in liftNameParts (:*:) namePartsL namePartsR

-- No instance for sums (:+:) since there is only a single default

instance GHasName names inner => GHasName names (M1 D meta inner) where
  gSplitName names (M1 inner) = M1 <$> gSplitName names inner

instance GHasName names inner => GHasName names (M1 C meta inner) where
  gSplitName names (M1 inner) = M1 <$> gSplitName names inner

class NameIfMember (names :: [Symbol]) (name :: Symbol) where
  nameIfMember :: proxy1 names -> proxy2 name -> Text -> Maybe Text

instance NameIfMember (name ': names) name where
  nameIfMember _ _ value = Just value

instance {-# OVERLAPPABLE #-} NameIfMember names name => NameIfMember (other ': names) name where
  nameIfMember _ _ _ = Nothing

instance NameIfMember '[] name where
  nameIfMember _ _ _ = Nothing

instance {-# OVERLAPPING #-} (KnownSymbol name, NameIfMember names name) => GHasName names (M1 S ('MetaSel ('Just name) su ss ds) (K1 index Text)) where
  gSplitName _ (M1 (K1 value)) = NameParts [value] (M1 $ K1 "") False

instance (HasEmpty value, Selector selector) => GHasName names (M1 S selector (K1 index value)) where
  gSplitName _ (M1 (K1 value)) = NameParts [] (M1 $ K1 value) (value == emptyValue)
