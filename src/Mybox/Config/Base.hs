module Mybox.Config.Base where

import Mybox.Config.Match
import Mybox.Driver
import Mybox.Package.Some
import Mybox.Prelude
import Mybox.Utils

data Config = Config
  { installSet :: Text
  , packages :: [SomePackage]
  }

defaultInstallSet :: Text
defaultInstallSet = "default"

inlineInstallSet :: Text
inlineInstallSet = "inline"

componentMatches :: Driver :> es => Match -> Eff es Bool
componentMatches m = do
  case m.host of
    Nothing -> pure True
    Just hosts -> do
      host <- drvHostname
      pure $ any (`glob` host) hosts

instance HasField "stateFilename" Config (Path Rel) where
  getField c = pSegment $ c.installSet <> ".yaml"
