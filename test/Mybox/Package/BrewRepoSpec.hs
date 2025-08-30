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
  onlyIfOS "Homebrew repository tests are only available on macOS" (\case MacOS -> True; _ -> False) $
    onlyIf "Homebrew repository tests require virtual system (Docker or CI)" virtualSystem $
      packageSpec $
        ps (mkBrewRepo "denji/nginx")
          & checkInstalledCommandOutput ("brew" :| ["info", "nginx-full"]) "HTTP(S) server"
