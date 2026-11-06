{-# LANGUAGE UndecidableInstances #-}

-- | Self-documenting JSON codecs.
--
-- A 'JSONCodec' instance bundles the encoder, the decoder and the documentation
-- of a type's JSON representation, so that the three cannot go out of sync.
--
-- Records are described field by field with 'jsonCodecObject'. Every field
-- specification is tagged with the Haskell field name, and the whole list is
-- matched against the record's 'Generic' representation, so adding, removing,
-- reordering or renaming a record field without updating the codec is a type
-- error:
--
-- > data Foo = Foo {bar :: Text, baz :: Text} deriving (Generic)
-- >   deriving (FromJSON, ToJSON) via ViaJSONCodec Foo
-- >
-- > instance JSONCodec Foo where
-- >   jsonCodec =
-- >     jsonCodecObject "Foo" $
-- >       field @"bar" "bar" "does some bar thing"
-- >         :* field @"baz" "bazRENAMED" "does some baz thing"
-- >         :* Nil
module Mybox.JSONCodec (
  -- * Classes
  JSONCodec (..),
  JSONObjectCodec (..),
  ViaJSONCodec (..),

  -- * Documentation
  ValueDoc (..),
  FieldDoc (..),

  -- * Value codecs
  ValueCodec (..),
  aesonCodec,
  enumCodec,
  objectCodec,

  -- * Object codecs
  ObjectCodec (..),
  jsonCodecObject,
  recordFields,
  Field,
  Fields (..),
  field,
  fieldDefault,
  fieldOptional,
  fieldFlat,
) where

import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import GHC.Generics hiding (to)
import GHC.Generics qualified as Generics
import GHC.TypeLits

import Mybox.Aeson
import Mybox.Prelude hiding (TypeError)

-- | Documentation of a JSON value.
data ValueDoc
  = DocText
  | DocBool
  | -- | One of the listed strings
    DocEnum [Text]
  | DocList ValueDoc
  | DocOneOf [ValueDoc]
  | -- | Object with the given name and fields
    DocObject Text [FieldDoc]
  deriving (Eq, Show)

-- | Documentation of a single field of a JSON object.
data FieldDoc = FieldDoc
  { key :: Key
  , description :: Text
  , required :: Bool
  , value :: ValueDoc
  }
  deriving (Eq, Show)

-- | Encoder, decoder and documentation for a JSON value.
data ValueCodec a = ValueCodec
  { to :: a -> Value
  , parse :: Value -> Parser a
  , doc :: ValueDoc
  }

-- | Encoder, decoder and documentation for a set of fields of a JSON object.
--
-- Unlike 'ValueCodec', several of these can be flattened into one object.
data ObjectCodec a = ObjectCodec
  { to :: a -> [Pair]
  , parse :: ObjectParser a
  , doc :: [FieldDoc]
  }

-- | Types with a documented JSON representation.
class JSONCodec a where
  jsonCodec :: ValueCodec a

-- | Types represented as a set of fields, to be flattened into an enclosing
-- object.
class JSONObjectCodec a where
  jsonObjectCodec :: ObjectCodec a

-- | Derive 'FromJSON' and 'ToJSON' from 'JSONCodec'.
newtype ViaJSONCodec a = ViaJSONCodec a

instance JSONCodec a => FromJSON (ViaJSONCodec a) where
  parseJSON = fmap ViaJSONCodec . jsonCodec.parse

instance JSONCodec a => ToJSON (ViaJSONCodec a) where
  toJSON (ViaJSONCodec a) = jsonCodec.to a

dimapObject :: (b -> a) -> (a -> b) -> ObjectCodec a -> ObjectCodec b
dimapObject f g c =
  ObjectCodec
    { to = c.to . f
    , parse = g <$> c.parse
    , doc = c.doc
    }

productObject :: (a -> b -> c) -> (c -> a) -> (c -> b) -> ObjectCodec a -> ObjectCodec b -> ObjectCodec c
productObject mk pa pb ca cb =
  ObjectCodec
    { to = \c -> ca.to (pa c) <> cb.to (pb c)
    , parse = mk <$> ca.parse <*> cb.parse
    , doc = ca.doc <> cb.doc
    }

-- | Codec using the existing Aeson instances, with the given documentation.
aesonCodec :: (FromJSON a, ToJSON a) => ValueDoc -> ValueCodec a
aesonCodec doc = ValueCodec{to = toJSON, parse = parseJSON, doc}

-- | Codec for an enumeration represented by strings.
enumCodec :: forall a. (Bounded a, Enum a, Eq a) => Text -> (a -> Text) -> ValueCodec a
enumCodec desc toText =
  ValueCodec
    { to = String . toText
    , parse = withText (Text.unpack desc) $ \t ->
        case find ((== t) . toText) values of
          Just v -> pure v
          Nothing -> fail $ "Unknown " <> Text.unpack desc <> ": " <> show t
    , doc = DocEnum $ map toText values
    }
 where
  values = [minBound .. maxBound]

-- | Codec for an object with the given name and fields. Unknown fields are
-- rejected.
objectCodec :: Text -> ObjectCodec a -> ValueCodec a
objectCodec name c =
  ValueCodec
    { to = object . c.to
    , parse = withObjectTotal name c.parse
    , doc = DocObject name c.doc
    }

instance JSONCodec Text where
  jsonCodec = aesonCodec DocText

instance JSONCodec Bool where
  jsonCodec = aesonCodec DocBool

instance Anchor a => JSONCodec (Path a) where
  jsonCodec = aesonCodec DocText

-- | Lists accept a single item in place of the list.
instance JSONCodec a => JSONCodec [a] where
  jsonCodec =
    ValueCodec
      { to = listValue c.to
      , parse = \case
          v@(Array _) -> listParser c.parse v
          v -> pure <$> c.parse v
      , doc = DocOneOf [c.doc, DocList c.doc]
      }
   where
    c = jsonCodec @a

instance JSONCodec a => JSONCodec (NonEmpty a) where
  jsonCodec =
    ValueCodec
      { to = list.to . toList
      , parse = list.parse >=> maybe (fail "Expected a non-empty list") pure . NonEmpty.nonEmpty
      , doc = list.doc
      }
   where
    list = jsonCodec @[a]

-- | Either shape as-is, without a tag. The 'Right' shape is tried first.
instance (JSONCodec a, JSONCodec b) => JSONCodec (Either a b) where
  jsonCodec =
    ValueCodec
      { to = either ca.to cb.to
      , parse = \v -> jsonAlternative (Right <$> cb.parse v) (Left <$> ca.parse v)
      , doc = DocOneOf [ca.doc, cb.doc]
      }
   where
    ca = jsonCodec @a
    cb = jsonCodec @b

-- | Specification of a record field named @name@ (on the Haskell side) of
-- type @a@.
newtype Field (name :: Symbol) a = Field (ObjectCodec a)

-- | Field specifications, in the record's order.
data Fields (fs :: [Type]) where
  Nil :: Fields '[]
  (:*) :: Field name a -> Fields fs -> Fields (Field name a ': fs)

infixr 5 :*

-- | Required field; always encoded.
field :: forall name a. JSONCodec a => Key -> Text -> Field name a
field key desc = fieldWith key desc jsonCodec

fieldWith :: forall name a. Key -> Text -> ValueCodec a -> Field name a
fieldWith key desc c =
  Field
    ObjectCodec
      { to = \v -> [key .= c.to v]
      , parse = takeFieldWith key c.parse
      , doc = [FieldDoc{key, description = desc, required = True, value = c.doc}]
      }

-- | Optional field; omitted when 'Nothing'. Absent and @null@ both decode
-- to 'Nothing'.
fieldOptional :: forall name a. JSONCodec a => Key -> Text -> Field name (Maybe a)
fieldOptional key desc = fieldOptionalWith key desc jsonCodec

fieldOptionalWith :: forall name a. Key -> Text -> ValueCodec a -> Field name (Maybe a)
fieldOptionalWith key desc c =
  Field
    ObjectCodec
      { to = \v -> [key .= c.to x | Just x <- [v]]
      , parse = takeFieldMaybeWith key c.parse
      , doc = [FieldDoc{key, description = desc, required = False, value = c.doc}]
      }

-- | Field defaulting to 'rvEmpty' when absent (or @null@); omitted when
-- 'rvNull'.
fieldDefault :: forall name a. (JSONCodec a, RecValue a) => Key -> Text -> Field name a
fieldDefault key desc = fieldDefaultWith key desc jsonCodec

fieldDefaultWith :: forall name a. RecValue a => Key -> Text -> ValueCodec a -> Field name a
fieldDefaultWith key desc c =
  let Field optional = fieldOptionalWith @name key desc c
   in Field $ dimapObject (\v -> if rvNull v then Nothing else Just v) (fromMaybe rvEmpty) optional

-- | Field whose own fields are flattened into the enclosing object.
fieldFlat :: forall name a. JSONObjectCodec a => Field name a
fieldFlat = Field jsonObjectCodec

-- | Match field specifications against a 'Generic' record representation,
-- consuming them from the front and returning the leftovers.
class GFields (f :: Type -> Type) (specs :: [Type]) (rest :: [Type]) | f specs -> rest where
  gFields :: Fields specs -> (ObjectCodec (f p), Fields rest)

instance GFields f specs rest => GFields (M1 D m f) specs rest where
  gFields = first (dimapObject unM1 M1) . gFields

instance GFields f specs rest => GFields (M1 C m f) specs rest where
  gFields = first (dimapObject unM1 M1) . gFields

instance GFields U1 specs specs where
  gFields specs = (ObjectCodec{to = const [], parse = pure U1, doc = []}, specs)

instance (GFields l specs mid, GFields r mid rest) => GFields (l :*: r) specs rest where
  gFields specs =
    let (cl, mid) = gFields specs
        (cr, rest) = gFields mid
     in (productObject (:*:) (\(x :*: _) -> x) (\(_ :*: y) -> y) cl cr, rest)

instance
  (a ~ a', name ~ name') =>
  GFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 i a)) (Field name' a' ': rest) rest
  where
  gFields (Field c :* rest) = (dimapObject (unK1 . unM1) (M1 . K1) c, rest)

instance
  TypeError ('Text "No JSON field specified for record field " ':<>: 'ShowType name) =>
  GFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 i a)) '[] '[]
  where
  gFields = error "unreachable"

-- | Codec for a record's fields. The specifications must match the record's
-- fields exactly, in order.
recordFields :: forall a specs. (GFields (Rep a) specs '[], Generic a) => Fields specs -> ObjectCodec a
recordFields = dimapObject from Generics.to . fst . gFields

-- | Codec for a record as a JSON object with the given name.
jsonCodecObject :: (GFields (Rep a) specs '[], Generic a) => Text -> Fields specs -> ValueCodec a
jsonCodecObject name = objectCodec name . recordFields
