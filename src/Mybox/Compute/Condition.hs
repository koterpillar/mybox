module Mybox.Compute.Condition where

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Compute.Facts
import Mybox.Driver
import Mybox.Prelude

data Conditions = Conditions
  { os :: Maybe [Text]
  , architecture :: Maybe [Architecture]
  , hostname :: Maybe [Text]
  }

instance FromJSON Conditions where
  parseJSON = withObjectTotal "Conditions" $ do
    os <- takeCollapsedListMaybe "os"
    architecture <- takeCollapsedListMaybe "architecture"
    hostname <- takeCollapsedListMaybe "hostname"
    pure Conditions{..}

match :: Driver :> es => Conditions -> Eff es Bool
match c =
  andM $
    toList (anyM architectureMatches <$> c.architecture)
      <> toList (anyM hostnameMatches <$> c.hostname)
      <> toList (anyM osMatch <$> c.os)

conditionProcessor :: Driver :> es => Value -> Eff es Bool
conditionProcessor value = do
  conditions <- parseThrow parseJSON value
  match conditions

ifProcessor :: Driver :> es => Processor (Eff es)
ifProcessor value rest = do
  result <- conditionProcessor value
  pure $ if result then Just (Object rest) else Nothing

unlessProcessor :: Driver :> es => Processor (Eff es)
unlessProcessor value rest = do
  result <- conditionProcessor value
  pure $ if result then Nothing else Just (Object rest)
