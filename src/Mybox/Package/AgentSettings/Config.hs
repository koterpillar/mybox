-- Settings objects retain key order because Kilo uses the last matching rule.
module Mybox.Package.AgentSettings.Config where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isSpace)
import Data.List (nub)
import Data.Text qualified as Text
import Data.Vector qualified as Vector

import Mybox.Aeson
import Mybox.Prelude

data Config = Fields [(Text, Config)] | Items [Config] | Scalar Value
  deriving (Eq, Show)

emptyConfig :: Config
emptyConfig = Fields []

fromValue :: Value -> Config
fromValue = \case
  Object fields -> Fields [(Key.toText k, fromValue v) | (k, v) <- KM.toList fields]
  Array items -> Items $ map fromValue $ Vector.toList items
  value -> Scalar value

toValue :: Config -> Value
toValue = \case
  Fields fields -> object [Key.fromText k .= toValue v | (k, v) <- fields]
  Items items -> toJSON $ map toValue items
  Scalar value -> value

renderConfig :: Config -> Text
renderConfig = \case
  Fields fields -> "{" <> Text.intercalate "," [jsonEncode k <> ":" <> renderConfig v | (k, v) <- fields] <> "}"
  Items items -> "[" <> Text.intercalate "," (map renderConfig items) <> "]"
  Scalar value -> jsonEncode value

textConfig :: Text -> Config
textConfig = Scalar . String

-- JSON with comments and trailing commas, as used by Kilo. Strings and
-- comment markers inside strings are passed unchanged to Aeson's decoder.
data Token = Punctuation Char | Atom Value deriving (Show)

parseConfig :: Text -> Either String Config
parseConfig input = do
  tokens <- lexConfig $ Text.unpack input
  (value, rest) <- parseValue tokens
  unless (null rest) $ Left "Unexpected content after settings object"
  pure value

lexConfig :: String -> Either String [Token]
lexConfig = \case
  [] -> pure []
  '/' : '/' : rest -> lexConfig $ dropWhile (/= '\n') rest
  '/' : '*' : rest -> blockComment rest >>= lexConfig
  '"' : rest -> do
    (literal, remaining) <- stringLiteral rest
    value <- eitherDecodeStrictText $ Text.pack $ '"' : literal
    (Atom value :) <$> lexConfig remaining
  c : rest
    | isSpace c -> lexConfig rest
    | c `elem` ("{}[],:" :: String) -> (Punctuation c :) <$> lexConfig rest
    | otherwise -> do
        let (literal, remaining) = span (\x -> not (isSpace x) && x `notElem` ("{}[],:/" :: String)) (c : rest)
        when (null literal) $ Left "Unexpected character in settings"
        value <- eitherDecodeStrictText $ Text.pack literal
        (Atom value :) <$> lexConfig remaining

blockComment :: String -> Either String String
blockComment = \case
  '*' : '/' : rest -> pure rest
  _ : rest -> blockComment rest
  [] -> Left "Unterminated settings comment"

stringLiteral :: String -> Either String (String, String)
stringLiteral = \case
  '"' : rest -> pure ("\"", rest)
  '\\' : c : rest -> first (('\\' :) . (c :)) <$> stringLiteral rest
  c : rest -> first (c :) <$> stringLiteral rest
  [] -> Left "Unterminated settings string"

parseValue :: [Token] -> Either String (Config, [Token])
parseValue = \case
  Punctuation '{' : rest -> first Fields <$> parseFields rest
  Punctuation '[' : rest -> first Items <$> parseItems rest
  Atom value : rest -> pure (Scalar value, rest)
  _ -> Left "Expected settings value"

parseFields :: [Token] -> Either String ([(Text, Config)], [Token])
parseFields = \case
  Punctuation '}' : rest -> pure ([], rest)
  Atom (String key) : Punctuation ':' : rest -> do
    (value, afterValue) <- parseValue rest
    (fields, remaining) <- case afterValue of
      Punctuation ',' : afterComma -> parseFields afterComma
      Punctuation '}' : afterBrace -> pure ([], afterBrace)
      _ -> Left "Expected comma or closing brace in settings"
    when (key `elem` map fst fields) $ Left "Duplicate settings key"
    pure ((key, value) : fields, remaining)
  _ -> Left "Expected settings key"

parseItems :: [Token] -> Either String ([Config], [Token])
parseItems = \case
  Punctuation ']' : rest -> pure ([], rest)
  tokens -> do
    (value, afterValue) <- parseValue tokens
    (items, remaining) <- case afterValue of
      Punctuation ',' : afterComma -> parseItems afterComma
      Punctuation ']' : afterBracket -> pure ([], afterBracket)
      _ -> Left "Expected comma or closing bracket in settings"
    pure (value : items, remaining)

modifyConfig :: [Text] -> (Config -> Either String Config) -> Config -> Either String Config
modifyConfig [] f value = f value
modifyConfig (key : rest) f (Fields fields) = do
  let existing = fromMaybe emptyConfig $ lookup key fields
  updated <- modifyConfig rest f existing
  pure
    $ Fields
    $ if key `elem` map fst fields
      then [(k, if k == key then updated else v) | (k, v) <- fields]
      else fields <> [(key, updated)]
modifyConfig _ _ _ = Left "Expected an object in existing settings"

appendItems :: [Config] -> Config -> Either String Config
appendItems new = \case
  Fields [] -> pure $ Items $ nub new
  Items existing -> pure $ Items $ nub $ existing <> new
  _ -> Left "Expected a list in existing settings"

-- New grants precede existing specific rules, so existing ask/deny rules
-- continue to win. A catch-all fallback must stay first.
allowPatterns :: [Text] -> Config -> Either String Config
allowPatterns patterns value = do
  fields <- case value of
    Fields fields -> pure fields
    Scalar (String "allow") -> pure [("*", textConfig "allow")]
    Scalar (String "ask") -> pure [("*", textConfig "ask")]
    _ -> Left "Cannot extend a denied or invalid tool permission"
  when (lookup "*" fields == Just (textConfig "deny")) $
    Left "Cannot extend a tool permission with an explicit catch-all denial"
  case break ((== "*") . fst) fields of
    (_ : _, _ : _) -> Left "Cannot extend tool permissions whose catch-all follows specific rules"
    _ -> pure ()
  let fallback = [(k, v) | (k, v) <- fields, k == "*"]
      specific = [(k, v) | (k, v) <- fields, k /= "*"]
      new = [(p, textConfig "allow") | p <- nub patterns, p `notElem` map fst fields]
  pure $ Fields $ fallback <> new <> specific
