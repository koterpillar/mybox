{-# LANGUAGE UndecidableInstances #-}

-- | Self-documenting JSON codecs.
--
-- A 'JSONEx' instance bundles the encoder, the decoder and the documentation
-- of a type's JSON representation, so that the three cannot go out of sync.
--
-- Records are described field by field with 'jsonExObject'. Every field
-- specification is tagged with the Haskell field name, and the whole list is
-- matched against the record's 'Generic' representation, so adding, removing,
-- reordering or renaming a record field without updating the codec is a type
-- error:
--
-- > data Foo = Foo {bar :: Text, baz :: Text} deriving (Generic)
-- >   deriving (FromJSON, ToJSON) via ViaJSONEx Foo
-- >
-- > instance JSONEx Foo where
-- >   jsonEx =
-- >     jsonExObject "Foo" $
-- >       field @"bar" "bar" "does some bar thing"
-- >         :* field @"baz" "bazRENAMED" "does some baz thing"
-- >         :* Nil
module Mybox.JSONEx (
  -- * Classes
  JSONEx (..),
  JSONExFields (..),
  ViaJSONEx (..),

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
  jsonExObject,
  recordFields,
  Field,
  Fields (..),
  field,
  fieldWith,
  fieldDefault,
  fieldDefaultWith,
  fieldOptional,
  fieldOptionalDefault,
  fieldNullable,
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
class JSONEx a where
  jsonEx :: ValueCodec a

-- | Types represented as a set of fields, to be flattened into an enclosing
-- object.
class JSONExFields a where
  jsonExFields :: ObjectCodec a

-- | Derive 'FromJSON' and 'ToJSON' from 'JSONEx'.
newtype ViaJSONEx a = ViaJSONEx a

instance JSONEx a => FromJSON (ViaJSONEx a) where
  parseJSON = fmap ViaJSONEx . jsonEx.parseValue

instance JSONEx a => ToJSON (ViaJSONEx a) where
  toJSON (ViaJSONEx a) = jsonEx.toValue a

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

instance JSONEx Text where
  jsonEx = aesonCodec DocText

instance JSONEx Bool where
  jsonEx = aesonCodec DocBool

instance Anchor a => JSONEx (Path a) where
  jsonEx = aesonCodec DocText

-- | Lists accept a single item in place of the list.
instance JSONEx a => JSONEx [a] where
  jsonEx = collapsedList jsonEx

instance JSONEx a => JSONEx (NonEmpty a) where
  jsonEx =
    ValueCodec
      { toValue = list.toValue . toList
      , parseValue = list.parseValue >=> maybe (fail "Expected a non-empty list") pure . NonEmpty.nonEmpty
      , valueDoc = list.valueDoc
      }
   where
    list = jsonEx @[a]

-- | Specification of a record field named @name@ (on the Haskell side) of
-- type @a@.
newtype Field (name :: Symbol) a = Field (ObjectCodec a)

-- | Field specifications, in the record's order.
data Fields (fs :: [Type]) where
  Nil :: Fields '[]
  (:*) :: Field name a -> Fields fs -> Fields (Field name a ': fs)

infixr 5 :*

-- | Required field; always encoded.
field :: forall name a. JSONEx a => Key -> Text -> Field name a
field key desc = fieldWith key desc jsonEx

fieldWith :: forall name a. Key -> Text -> ValueCodec a -> Field name a
fieldWith key desc c =
  Field
    ObjectCodec
      { toPairs = \v -> [key .= c.toValue v]
      , parseFields = takeFieldWith key c.parseValue
      , fieldDocs = [FieldDoc{key, description = desc, required = True, value = c.valueDoc}]
      }

-- | Field with a default for when it is absent (or @null@); always encoded.
fieldDefault :: forall name a. JSONEx a => Key -> Text -> a -> Field name a
fieldDefault key desc def = fieldDefaultWith key desc def jsonEx

fieldDefaultWith :: forall name a. Key -> Text -> a -> ValueCodec a -> Field name a
fieldDefaultWith key desc def c =
  Field
    ObjectCodec
      { toPairs = \v -> [key .= c.toValue v]
      , parseFields = fromMaybe def <$> takeFieldMaybeWith key c.parseValue
      , fieldDocs = [FieldDoc{key, description = desc, required = False, value = c.valueDoc}]
      }

-- | Optional field; omitted when 'Nothing'.
fieldOptional :: forall name a. JSONEx a => Key -> Text -> Field name (Maybe a)
fieldOptional key desc = fieldOptionalWith key desc jsonEx

fieldOptionalWith :: forall name a. Key -> Text -> ValueCodec a -> Field name (Maybe a)
fieldOptionalWith key desc c =
  Field
    ObjectCodec
      { toPairs = \v -> [key .= c.toValue x | Just x <- [v]]
      , parseFields = takeFieldMaybeWith key c.parseValue
      , fieldDocs = [FieldDoc{key, description = desc, required = False, value = c.valueDoc}]
      }

-- | Field with a default; omitted when equal to the default.
fieldOptionalDefault :: forall name a. (Eq a, JSONEx a) => Key -> Text -> a -> Field name a
fieldOptionalDefault key desc def =
  let Field c = fieldOptionalWith @name key desc jsonEx
   in Field $ dimapObject (\v -> if v == def then Nothing else Just v) (fromMaybe def) c

-- | Optional field; encoded as @null@ when 'Nothing'.
fieldNullable :: forall name a. JSONEx a => Key -> Text -> Field name (Maybe a)
fieldNullable key desc =
  Field
    ObjectCodec
      { toPairs = \v -> [key .= maybe Null c.toValue v]
      , parseFields = takeFieldMaybeWith key c.parseValue
      , fieldDocs = [FieldDoc{key, description = desc, required = False, value = c.valueDoc}]
      }
 where
  c = jsonEx @a

-- | Field whose own fields are flattened into the enclosing object.
fieldFlat :: forall name a. JSONExFields a => Field name a
fieldFlat = Field jsonExFields

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
jsonExObject :: (GFields (Rep a) specs '[], Generic a) => Text -> Fields specs -> ValueCodec a
jsonExObject name = objectCodec name . recordFields
