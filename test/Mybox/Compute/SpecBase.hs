module Mybox.Compute.SpecBase where

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Driver
import Mybox.Driver.Test
import Mybox.Prelude

mockPlatform :: Architecture -> OS -> Args -> Maybe Text
mockPlatform arch _ ("uname" :| ["-m"]) = Just $ architectureString arch
mockPlatform _ _ ("uname" :| ["-n"]) = Just "my-host"
mockPlatform _ os ("uname" :| []) = Just $ case os of
  MacOS -> "Darwin"
  Linux _ -> "Linux"
mockPlatform _ (Linux (Debian variant)) ("cat" :| ["/etc/os-release"]) =
  Just $ "ID=" <> variant <> "\n"
mockPlatform _ (Linux Fedora) ("cat" :| ["/etc/os-release"]) =
  Just "ID=fedora\n"
mockPlatform _ _ _ = Nothing

runMockPlatform :: Architecture -> OS -> Eff '[Driver] r -> r
runMockPlatform !arch !os = runPureEff . pureDriver (mockPlatform arch os)

runProcessor :: Architecture -> OS -> Value -> Object -> Processor (Eff '[Driver]) -> Maybe Value
runProcessor !arch !os value rest processor = runMockPlatform arch os $ processor value rest
