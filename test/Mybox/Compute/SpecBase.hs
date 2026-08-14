module Mybox.Compute.SpecBase where

import Data.Map qualified as Map

import Mybox.Aeson
import Mybox.Compute
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

-- | The processor for a sigil as it is enabled in the application, so that
-- implementing a processor without registering it fails the tests.
sigilProcessor :: Text -> Processor (Eff '[Driver])
sigilProcessor sigil =
  fromMaybe (terror $ "Sigil not enabled: " <> sigil) $ Map.lookup sigil sigils

-- | Run a sigil's processor, mocking the commands it runs.
runProcessorWith :: (Args -> Maybe Text) -> Value -> Object -> Text -> Maybe Value
runProcessorWith mock value rest sigil = runPureEff $ pureDriver mock $ sigilProcessor sigil value rest

runProcessor :: Architecture -> OS -> Value -> Object -> Text -> Maybe Value
runProcessor !arch !os = runProcessorWith (mockPlatform arch os)

-- | Run a sigil's processor that is not expected to run any commands.
runPureProcessor :: Value -> Object -> Text -> Maybe Value
runPureProcessor = runProcessorWith $ const Nothing
