module Mybox.Config (
  module Mybox.Config.Reader,
  Config (..),
  readConfig,
  runReaderIO,
) where

import Mybox.Config.IO (runReaderIO)
import Mybox.Config.Match
import Mybox.Config.Reader
import Mybox.Package.Some
import Mybox.Prelude
import Mybox.Utils

newtype Config = Config {packages :: [SomePackage]}

componentMatches :: Reader :> es => Match -> Eff es Bool
componentMatches m = do
  case m.host of
    Nothing -> pure True
    Just hosts -> do
      host <- readHost
      pure $ any (`glob` host) hosts

readConfig :: Reader :> es => Eff es Config
readConfig = do
  rootConfig <- readConfigYAML "mybox.yaml"
  matches <- filterM componentMatches rootConfig
  packages <- fmap (join . join) $
    for matches $ \match ->
      for match.component $ \component ->
        readConfigYAML ("packages" </> component <> ".yaml")
  pure $ Config{..}
