module Mybox.Compute.Switch where

import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Compute.Facts
import Mybox.Driver
import Mybox.Prelude

data SwitchKey = SKArchitecture | SKOS | SKHostname
  deriving (Eq, Show)

instance FromJSON SwitchKey where
  parseJSON = withText "SwitchKey" $ \case
    "architecture" -> pure SKArchitecture
    "os" -> pure SKOS
    "hostname" -> pure SKHostname
    _ -> fail "Invalid switch key"

switchProcessor :: Driver :> es => Processor (Eff es)
switchProcessor value rest = do
  key <- parseThrow parseJSON value
  let matcher = case key of
        SKArchitecture -> architectureMatches
        SKOS -> osMatch
        SKHostname -> hostnameMatches
  matchingKeys <- filterM (matcher . K.toText . fst) (KM.toList rest)
  pure $ case matchingKeys of
    [] -> Nothing
    [(_, v)] -> Just v
    _ -> terror $ "Multiple matching keys found for switch: " <> Text.pack (show $ map fst matchingKeys)
