module Mybox.Compute.Facts where

import Mybox.Driver
import Mybox.Prelude
import Mybox.Utils

andM :: Monad m => [m Bool] -> m Bool
andM = foldM go True
 where
  go False _ = pure False
  go True act = act

architectureMatches :: Driver :> es => [Architecture] -> Eff es Bool
architectureMatches as = flip elem as <$> drvArchitecture

hostnameMatches :: Driver :> es => [Text] -> Eff es Bool
hostnameMatches hostnames = do
  hostname <- drvHostname
  pure $ any (`glob` hostname) hostnames

osMatches :: Driver :> es => [Text] -> Eff es Bool
osMatches os = flip any os . matches <$> drvOS
 where
  matches MacOS "darwin" = True
  matches (Linux _) "linux" = True
  matches (Linux Fedora) "fedora" = True
  matches (Linux (Debian variant)) s = s == variant
  matches _ _ = False
