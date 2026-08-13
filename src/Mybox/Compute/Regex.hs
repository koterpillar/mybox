module Mybox.Compute.Regex where

import Text.Regex.TDFA
import Text.Regex.TDFA.Text qualified as Regex

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Filters
import Mybox.Prelude

newtype Pattern = Pattern Regex

instance FromJSON Pattern where
  parseJSON = withText "Pattern" $ \p ->
    case Regex.compile defaultCompOpt defaultExecOpt p of
      Left err -> fail $ "invalid regex " <> show p <> ": " <> err
      Right regex -> pure $ Pattern regex

patternMatches :: Pattern -> Text -> [Text]
patternMatches (Pattern regex) base = map firstGroup (match regex base :: [[Text]])
 where
  firstGroup (whole : groups) = fromMaybe whole $ listToMaybe groups
  firstGroup [] = terror "no match groups"

regexProcessor :: Processor (Eff es)
regexProcessor patternValue rest = do
  pattern_ <- parseThrow parseJSON patternValue
  (base, args) <-
    flip parseThrow rest
      $ parseObjectTotal
      $ (,) <$> takeField "base" <*> takeFilter
  result <- throwLeft $ choose_ (toFilters args) $ patternMatches pattern_ base
  pure $ Just $ String result
