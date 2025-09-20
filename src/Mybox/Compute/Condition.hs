module Mybox.Compute.Condition where

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Driver
import Mybox.Prelude

data Conditions = Conditions
  { os :: Maybe [Text]
  , architecture :: Maybe [Architecture]
  }
  deriving (Eq, Ord, Show)

instance FromJSON Conditions where
  parseJSON = withObjectTotal "Conditions" $ do
    os <- takeCollapsedListMaybe "os"
    architecture <- takeCollapsedListMaybe "architecture"
    pure Conditions{..}

andM :: Monad m => [m Bool] -> m Bool
andM = foldM go True
 where
  go False _ = pure False
  go True act = act

orM :: Monad m => [m Bool] -> m Bool
orM = foldM go False
 where
  go True _ = pure True
  go False act = act

match :: Driver :> es => Conditions -> Eff es Bool
match c = andM $ toList (osMatches <$> c.os) <> toList (architectureMatches <$> c.architecture)

architectureMatches :: Driver :> es => [Architecture] -> Eff es Bool
architectureMatches as = flip elem as <$> drvArchitecture

osMatches :: Driver :> es => [Text] -> Eff es Bool
osMatches os = flip any os . matches <$> drvOS
 where
  matches MacOS "darwin" = True
  matches (Linux _) "linux" = True
  matches (Linux Fedora) "fedora" = True
  matches (Linux (Debian variant)) s = s == variant
  matches _ _ = False

conditionProcessor :: Driver :> es => Processor (Eff es)
conditionProcessor value rest = do
  conditions <- parseThrow parseJSON value
  result <- match conditions
  pure $ if result then Just (Object rest) else Nothing
