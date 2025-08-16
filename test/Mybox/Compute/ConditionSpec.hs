module Mybox.Compute.ConditionSpec where

import Mybox.Aeson
import Mybox.Compute.Condition
import Mybox.Driver
import Mybox.Driver.Test
import Mybox.Prelude
import Mybox.SpecBase

mockPlatform :: Architecture -> OS -> Args -> Maybe Text
mockPlatform arch _ ("uname" :| ["-m"]) = Just $ architectureString arch
mockPlatform _ os ("uname" :| []) = Just $ case os of
  MacOS -> "Darwin"
  Linux _ -> "Linux"
mockPlatform _ (Linux (Debian variant)) ("cat" :| ["/etc/os-release"]) =
  Just $ "ID=" <> variant <> "\n"
mockPlatform _ (Linux Fedora) ("cat" :| ["/etc/os-release"]) =
  Just "ID=fedora\n"
mockPlatform _ _ _ = Nothing

run :: Architecture -> OS -> Value -> Object -> Maybe Value
run arch os value base = runPureEff $ pureDriver (mockPlatform arch os) $ conditionProcessor value base

rest :: Object
rest = "rest" .= String "value"

shouldMatch :: IOE :> es => Architecture -> OS -> Value -> Eff es ()
shouldMatch arch os value = run arch os value rest `shouldBe` Just (Object rest)

shouldNotMatch :: IOE :> es => Architecture -> OS -> Value -> Eff es ()
shouldNotMatch arch os value = run arch os value rest `shouldBe` Nothing

spec :: Spec
spec = do
  describe "conditionProcessor" $ do
    it "passes through when no OS condition is specified" $ do
      shouldMatch X86_64 MacOS (object [])
      shouldMatch Aarch64 (Linux Fedora) (object [])

    it "filters on OS" $ do
      let value = object ["os" .= ("darwin" :: Text)]
      shouldMatch X86_64 MacOS value
      shouldNotMatch X86_64 (Linux Fedora) value

    it "filters on architecture" $ do
      let value = object ["architecture" .= ("x86_64" :: Text)]
      shouldMatch X86_64 MacOS value
      shouldNotMatch Aarch64 MacOS value

    it "accepts any element from the list" $ do
      let value = object ["os" .= ["darwin" :: Text, "debian"]]
      shouldMatch X86_64 MacOS value
      shouldMatch Aarch64 (Linux (Debian "debian")) value
      shouldNotMatch Aarch64 (Linux Fedora) value

    it "checks all conditions" $ do
      let value =
            object
              [ "os" .= ("darwin" :: Text)
              , "architecture" .= ("x86_64" :: Text)
              ]
      shouldMatch X86_64 MacOS value
      shouldNotMatch Aarch64 MacOS value
      shouldNotMatch X86_64 (Linux Fedora) value
