module Mybox.Config.Base where

import Mybox.Config.Match
import Mybox.Driver
import Mybox.Package.Some
import Mybox.Prelude
import Mybox.Utils

newtype Config = Config
  { packages :: [SomePackage]
  }

componentMatches :: Driver :> es => Match -> Eff es Bool
componentMatches m = do
  case m.host of
    Nothing -> pure True
    Just hosts -> do
      host <- drvHostname
      pure $ any (`glob` host) hosts
