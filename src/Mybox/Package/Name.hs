{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Package.Name where

import Data.Text qualified as Text
import Data.Type.Equality hiding (inner)
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
joinName prefix parts = Text.intercalate "#" $ maybe id (:) prefix $ filter (not . Text.null) parts

genericSplitName :: (GHasName '["name"] (Rep a), Generic a) => a -> (Text, Maybe a)
genericSplitName = genericSplitName' @'[] @'["name"]

genericSplitName' :: forall (prefix :: [Symbol]) names a. (GHasName names (Rep a), Generic a, KnownMaybeSymbol prefix) => a -> (Text, Maybe a)
genericSplitName' value =
  let r = gSplitName @names $ from value
      name = joinName (Text.pack <$> symbolValMaybe @prefix) r.parts
      rest = if r.allDefault then Nothing else Just $ to r.rest
   in (name, rest)

data NameParts p = NameParts {parts :: [Text], rest :: p, allDefault :: Bool}
  deriving (Functor)

liftNameParts :: (a -> b -> c) -> NameParts a -> NameParts b -> NameParts c
liftNameParts f (NameParts partsL restL allDefL) (NameParts partsR restR allDefR) =
  NameParts (partsL <> partsR) (f restL restR) (allDefL && allDefR)

class GHasName (names :: [Symbol]) f where
  gSplitName :: f p -> NameParts (f p)

-- No instance for U1 since there must be at least one field for the package name

instance (GHasName names left, GHasName names right) => GHasName names (left :*: right) where
  gSplitName (left :*: right) =
    let namePartsL = gSplitName @names left
        namePartsR = gSplitName @names right
     in liftNameParts (:*:) namePartsL namePartsR

-- No instance for sums (:+:) since there is only a single default

instance GHasName names inner => GHasName names (M1 D meta inner) where
  gSplitName (M1 inner) = M1 <$> gSplitName @names inner

instance GHasName names inner => GHasName names (M1 C meta inner) where
  gSplitName (M1 inner) = M1 <$> gSplitName @names inner

instance
  (KnownSymbol name, KnownSymbols names, RecValue value) =>
  GHasName names (M1 S ('MetaSel ('Just name) su ss ds) (K1 index value))
  where
  gSplitName (M1 (K1 value))
    | memberSymbol (Proxy @names) (Proxy @name) = case rvText value of
        Just name -> NameParts [name] (M1 $ K1 rvEmpty) True
        Nothing -> error $ "name field " <> symbolVal (Proxy @name) <> " is not text"
    | otherwise = NameParts [] (M1 $ K1 value) (rvNull value)

class KnownSymbols (as :: [Symbol]) where
  memberSymbol :: forall b proxy1 proxy2. KnownSymbol b => proxy1 as -> proxy2 b -> Bool

instance KnownSymbols '[] where
  memberSymbol _ _ = False

instance (KnownSymbol a, KnownSymbols as) => KnownSymbols (a ': as) where
  memberSymbol _ pb = case sameSymbol (Proxy @a) pb of
    Just Refl -> True
    Nothing -> memberSymbol (Proxy @as) pb

class KnownMaybeSymbol (a :: [Symbol]) where
  symbolValMaybe :: Maybe String

instance KnownMaybeSymbol '[] where
  symbolValMaybe = Nothing

instance KnownSymbol a => KnownMaybeSymbol '[a] where
  symbolValMaybe = Just (symbolVal (Proxy @a))
