module Mybox.Config (
  Config (..),
  getConfig,
) where

import Mybox.Aeson
import Mybox.Compute
import Mybox.Config.Base
import Mybox.Config.CommandLine
import Mybox.Config.Match
import Mybox.Driver
import Mybox.Prelude

readYAML :: (Anchor a, Driver :> es, FromJSON r) => Path a -> Eff es r
readYAML p =
  drvReadFile p
    >>= yamlDecode
    >>= parseThrow (parseJSONWithContext p.text)

readConfig :: Driver :> es => CommandLine -> Eff es Config
readConfig _ = do
  let configDirectory = pWiden pCurrent
  rootConfig <- readYAML $ configDirectory </> "mybox.yaml"
  matches <- filterM componentMatches rootConfig
  packages <- fmap (join . join) $
    for matches $ \match ->
      for match.component $ \component -> do
        raw <- readYAML (configDirectory </> "packages" </> (component <> ".yaml"))
        componentPackages <- preprocess raw
        parseThrow parseJSON componentPackages
  pure $ Config{..}

getConfig :: (Driver :> es, IOE :> es) => Eff es Config
getConfig = do
  options <- parseArgs
  readConfig options
