module Mybox.Compute.Format where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Prelude
import Mybox.Utils (throwLeft)

parseBase :: Object -> Parser [Text]
parseBase o = parseCollapsedList o "base"

doFormat :: Text -> [Text] -> Either String Text
doFormat str values =
  let (before, after) = Text.breakOn "{}" str
   in case (values, after) of
        ([], "") -> pure before
        ([], _) -> Left "No more values but placeholders remain"
        (_, "") -> Left "No more placeholders but values remain"
        (v : vs, _) -> (\r -> before <> v <> r) <$> doFormat (Text.drop 2 after) vs

formatProcessor :: Processor (Eff es)
formatProcessor formatValue baseValue = do
  format <- parseThrow parseJSON formatValue
  bases <- parseThrow parseBase baseValue
  result <- throwLeft $ doFormat format bases
  pure $ String result
