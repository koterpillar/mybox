module Mybox.Package.BrewRepoSpec where

import Mybox.Driver
import Mybox.Package.BrewRepo
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    (Nothing @BrewRepo)
    [ (Nothing, "{\"brew_tap\": \"denji/nginx\"}")
    ]
  onlyIfOS (\case MacOS -> True; _ -> False) $
    packageSpec $ \psa ->
      ps
        (mkBrewRepo "denji/nginx")
        & checkInstalledCommandOutput ("brew" :| ["info", "nginx-full"]) "HTTP(S) server"
        & psPendingIf (not psa.virtualSystem)
