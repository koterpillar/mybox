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
  collapsedList,
  collapsedEither,
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
import GHC.Generics
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
  { toValue :: a -> Value
  , parseValue :: Value -> Parser a
  , valueDoc :: ValueDoc
  }

-- | Encoder, decoder and documentation for a set of fields of a JSON object.
--
-- Unlike 'ValueCodec', several of these can be flattened into one object.
data ObjectCodec a = ObjectCodec
  { toPairs :: a -> [Pair]
  , parseFields :: ObjectParser a
  , fieldDocs :: [FieldDoc]
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
  parseJSON = fmap ViaJSONCodec . jsonCodec.parseValue

instance JSONCodec a => ToJSON (ViaJSONCodec a) where
  toJSON (ViaJSONCodec a) = jsonCodec.toValue a

dimapObject :: (b -> a) -> (a -> b) -> ObjectCodec a -> ObjectCodec b
dimapObject f g c =
  ObjectCodec
    { toPairs = c.toPairs . f
    , parseFields = g <$> c.parseFields
    , fieldDocs = c.fieldDocs
    }

productObject :: (a -> b -> c) -> (c -> a) -> (c -> b) -> ObjectCodec a -> ObjectCodec b -> ObjectCodec c
productObject mk pa pb ca cb =
  ObjectCodec
    { toPairs = \c -> ca.toPairs (pa c) <> cb.toPairs (pb c)
    , parseFields = mk <$> ca.parseFields <*> cb.parseFields
    , fieldDocs = ca.fieldDocs <> cb.fieldDocs
    }

-- | Codec using the existing Aeson instances, with the given documentation.
aesonCodec :: (FromJSON a, ToJSON a) => ValueDoc -> ValueCodec a
aesonCodec doc = ValueCodec{toValue = toJSON, parseValue = parseJSON, valueDoc = doc}

-- | Codec for an enumeration represented by strings.
enumCodec :: forall a. (Bounded a, Enum a, Eq a) => Text -> (a -> Text) -> ValueCodec a
enumCodec desc toText =
  ValueCodec
    { toValue = String . toText
    , parseValue = withText (Text.unpack desc) $ \t ->
        case find ((== t) . toText) values of
          Just v -> pure v
          Nothing -> fail $ "Unknown " <> Text.unpack desc <> ": " <> show t
    , valueDoc = DocEnum $ map toText values
    }
 where
  values = [minBound .. maxBound]

-- | Codec for a list that also accepts a single item in place of the list.
collapsedList :: ValueCodec a -> ValueCodec [a]
collapsedList c =
  ValueCodec
    { toValue = listValue c.toValue
    , parseValue = \case
        v@(Array _) -> listParser c.parseValue v
        v -> pure <$> c.parseValue v
    , valueDoc = DocOneOf [c.valueDoc, DocList c.valueDoc]
    }

-- | Codec for a value that can take one of two shapes. The 'Right' shape is
-- tried first.
collapsedEither :: ValueCodec a -> ValueCodec b -> ValueCodec (Either a b)
collapsedEither ca cb =
  ValueCodec
    { toValue = either ca.toValue cb.toValue
    , parseValue = \v -> jsonAlternative (Right <$> cb.parseValue v) (Left <$> ca.parseValue v)
    , valueDoc = DocOneOf [ca.valueDoc, cb.valueDoc]
    }

-- | Codec for an object with the given name and fields. Unknown fields are
-- rejected.
objectCodec :: Text -> ObjectCodec a -> ValueCodec a
objectCodec name c =
  ValueCodec
    { toValue = object . c.toPairs
    , parseValue = withObjectTotal (Text.unpack name) c.parseFields
    , valueDoc = DocObject name c.fieldDocs
    }

instance JSONCodec Text where
  jsonCodec = aesonCodec DocText

instance JSONCodec Bool where
  jsonCodec = aesonCodec DocBool

instance Anchor a => JSONCodec (Path a) where
  jsonCodec = aesonCodec DocText

-- | Lists accept a single item in place of the list.
instance JSONCodec a => JSONCodec [a] where
  jsonCodec = collapsedList jsonCodec

instance JSONCodec a => JSONCodec (NonEmpty a) where
  jsonCodec =
    ValueCodec
      { toValue = list.toValue . toList
      , parseValue = list.parseValue >=> maybe (fail "Expected a non-empty list") pure . NonEmpty.nonEmpty
      , valueDoc = list.valueDoc
      }
   where
    list = jsonCodec @[a]

-- | Either shape as-is, without a tag. The 'Right' shape is tried first.
instance (JSONCodec a, JSONCodec b) => JSONCodec (Either a b) where
  jsonCodec = collapsedEither jsonCodec jsonCodec

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
      { toPairs = \v -> [key .= c.toValue v]
      , parseFields = takeFieldWith key c.parseValue
      , fieldDocs = [FieldDoc{key, description = desc, required = True, value = c.valueDoc}]
      }

-- | Optional field; omitted when 'Nothing'. Absent and @null@ both decode
-- to 'Nothing'.
fieldOptional :: forall name a. JSONCodec a => Key -> Text -> Field name (Maybe a)
fieldOptional key desc = fieldOptionalWith key desc jsonCodec

fieldOptionalWith :: forall name a. Key -> Text -> ValueCodec a -> Field name (Maybe a)
fieldOptionalWith key desc c =
  Field
    ObjectCodec
      { toPairs = \v -> [key .= c.toValue x | Just x <- [v]]
      , parseFields = takeFieldMaybeWith key c.parseValue
      , fieldDocs = [FieldDoc{key, description = desc, required = False, value = c.valueDoc}]
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
  gFields specs = (ObjectCodec{toPairs = const [], parseFields = pure U1, fieldDocs = []}, specs)

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
recordFields = dimapObject from to . fst . gFields

-- | Codec for a record as a JSON object with the given name.
jsonCodecObject :: (GFields (Rep a) specs '[], Generic a) => Text -> Fields specs -> ValueCodec a
jsonCodecObject name = objectCodec name . recordFields
