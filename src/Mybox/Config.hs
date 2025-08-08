module Mybox.Config (
  Config (..),
  readConfig,
) where

import Mybox.Aeson
import Mybox.Config.Match
import Mybox.Driver
import Mybox.Package.Some
import Mybox.Prelude
import Mybox.Utils

newtype Config = Config {packages :: [SomePackage]}

componentMatches :: Driver :> es => Match -> Eff es Bool
componentMatches m = do
  case m.host of
    Nothing -> pure True
    Just hosts -> do
      host <- drvHostname
      pure $ any (`glob` host) hosts

readYAML :: (Anchor a, Driver :> es, FromJSON r) => Path a -> Eff es r
readYAML p =
  drvReadFile p
    >>= yamlDecode
    >>= parseThrow (parseJSONWithContext p.text)

readConfig :: Driver :> es => Eff es Config
readConfig = do
  rootConfig <- readYAML $ pSegment "mybox.yaml"
  matches <- filterM componentMatches rootConfig
  packages <- fmap (join . join) $
    for matches $ \match ->
      for match.component $ \component ->
        readYAML (pSegment "packages" </> (component <> ".yaml"))
  pure $ Config{..}
