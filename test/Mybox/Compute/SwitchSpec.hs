module Mybox.Compute.SwitchSpec where

import Mybox.Aeson
import Mybox.Compute.SpecBase
import Mybox.Compute.Switch
import Mybox.Driver
import Mybox.Prelude
import Mybox.SpecBase

run :: Architecture -> OS -> Text -> Object -> Maybe Value
run !arch !os value rest = runProcessor arch os (String value) rest switchProcessor

spec :: Spec
spec = do
  describe "switchProcessor" $ do
    it "selects architecture" $ do
      run X86_64 MacOS "architecture" ("x86_64" .= String "result") `shouldBe` Just "result"
      run Aarch64 MacOS "architecture" ("aarch64" .= String "result") `shouldBe` Just (String "result")
      run Aarch64 MacOS "architecture" ("arm64" .= String "result") `shouldBe` Just (String "result")

    it "selects from multiple items" $ do
      let items = "x86_64" .= String "one" <> "aarch64" .= String "two"
      run X86_64 MacOS "architecture" items `shouldBe` Just (String "one")
      run Aarch64 MacOS "architecture" items `shouldBe` Just (String "two")

    it "returns Nothing when there is no match" $ do
      run X86_64 MacOS "architecture" ("aarch64" .= String "result") `shouldBe` Nothing
      run Aarch64 MacOS "architecture" ("x86_64" .= String "result") `shouldBe` Nothing

    it "selects OS" $ do
      let items =
            mconcat
              [ "darwin" .= String "one"
              , "fedora" .= String "two"
              , "debian" .= String "three"
              ]
      run X86_64 MacOS "os" items `shouldBe` Just (String "one")
      run X86_64 (Linux Fedora) "os" items `shouldBe` Just (String "two")
      run X86_64 (Linux (Debian "debian")) "os" items `shouldBe` Just (String "three")

    it "selects hostname" $ do
      let items = "my-host" .= String "one" <> "other-host" .= String "two"
      run X86_64 MacOS "hostname" items `shouldBe` Just (String "one")

    it "errors when multiple matches are found" $ do
      let items = "linux" .= String "one" <> "fedora" .= String "two"
      evaluate (run X86_64 (Linux Fedora) "os" items) `shouldThrow` anyErrorCall
