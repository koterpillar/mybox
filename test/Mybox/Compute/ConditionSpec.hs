module Mybox.Compute.ConditionSpec where

import Mybox.Aeson
import Mybox.Compute.Condition
import Mybox.Compute.SpecBase
import Mybox.Driver
import Mybox.Prelude
import Mybox.SpecBase

rest :: Object
rest = "rest" .= String "value"

shouldMatch :: IOE :> es => Architecture -> OS -> Value -> Eff es ()
shouldMatch !arch !os value = do
  runProcessor arch os value rest ifProcessor `shouldBe` Just (Object rest)
  runProcessor arch os value rest unlessProcessor `shouldBe` Nothing

shouldNotMatch :: IOE :> es => Architecture -> OS -> Value -> Eff es ()
shouldNotMatch !arch !os value = do
  runProcessor arch os value rest ifProcessor `shouldBe` Nothing
  runProcessor arch os value rest unlessProcessor `shouldBe` Just (Object rest)

spec :: Spec
spec = do
  describe "ifProcessor" $ do
    it "passes through when no OS condition is specified" $ do
      shouldMatch X86_64 MacOS (object [])
      shouldMatch Aarch64 (Linux Fedora) (object [])

    it "filters OS" $ do
      let value = object ["os" .= ("darwin" :: Text)]
      shouldMatch X86_64 MacOS value
      shouldNotMatch X86_64 (Linux Fedora) value

    it "filters on architecture" $ do
      let value = object ["architecture" .= ("x86_64" :: Text)]
      shouldMatch X86_64 MacOS value
      shouldNotMatch Aarch64 MacOS value

    it "filters on hostname" $ do
      shouldMatch X86_64 MacOS (object ["hostname" .= ("my-*" :: Text)])
      shouldMatch X86_64 MacOS (object ["hostname" .= ("*-host" :: Text)])
      shouldNotMatch X86_64 MacOS (object ["hostname" .= ("other-*" :: Text)])

    it "accepts any hostname from the list" $ do
      let value = object ["hostname" .= ["other-host" :: Text, "my-host"]]
      shouldMatch X86_64 MacOS value
      shouldNotMatch X86_64 MacOS (object ["hostname" .= ["a" :: Text, "b"]])

    it "checks all conditions" $ do
      let value =
            object
              [ "os" .= ("darwin" :: Text)
              , "architecture" .= ("x86_64" :: Text)
              ]
      shouldMatch X86_64 MacOS value
      shouldNotMatch Aarch64 MacOS value
      shouldNotMatch X86_64 (Linux Fedora) value
