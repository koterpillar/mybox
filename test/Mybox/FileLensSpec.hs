module Mybox.FileLensSpec where

import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Vector qualified as Vector
import Data.Yaml qualified as Yaml

import Mybox.Driver
import Mybox.FileLens
import Mybox.Prelude
import Mybox.SpecBase

writePermissions :: Traversal' Value Array
writePermissions = keyWithDefault "permissions" (Object mempty) . keyWithDefault "write" (Array mempty) . _Array

fontSize :: Traversal' Value (Maybe Value)
fontSize = keyWithDefault "font" (Object mempty) . atKey "size"

appendWorkspace :: Array -> Array
appendWorkspace = (`Vector.snoc` String "workspace")

decodeYaml :: ByteString -> Maybe Value
decodeYaml = either (const Nothing) Just . Yaml.decodeEither'

modifyFile :: ByteString -> (Maybe ByteString -> ByteString) -> Eff BaseEff ByteString
modifyFile input modify = drvTempFile $ \path -> do
  drvWriteBinaryFile path $ LBS.fromStrict input
  drvModifyBinaryFile path modify
  drvReadBinaryFile path

spec :: Spec
spec = do
  describe "appending a write permission" $ do
    let append = jsonBytes . writePermissions %~ appendWorkspace
    for_
      [ ("creates the permissions object and write array", "{\"other\":true}", "{\"other\":true,\"permissions\":{\"write\":[\"workspace\"]}}")
      , ("creates a missing write array", "{\"permissions\":{\"read\":[\"home\"]}}", "{\"permissions\":{\"read\":[\"home\"],\"write\":[\"workspace\"]}}")
      , ("appends to the existing array", "{\"permissions\":{\"write\":[\"home\"]}}", "{\"permissions\":{\"write\":[\"home\",\"workspace\"]}}")
      ]
      $ \(name, input, expected) -> it name $ do
        output <- modifyFile input append
        decodeStrict @Value output `shouldBe` decodeStrict expected
    for_
      [ ("preserves invalid JSON", "not JSON\n")
      , ("preserves a non-object root", " [1] \n")
      , ("preserves a non-object permissions field", "{ \"permissions\": null }\n")
      , ("preserves a non-array write field", "{ \"permissions\": { \"write\": true } }\n")
      ]
      $ \(name, input) -> it name $ modifyFile input append >>= (`shouldBe` input)
  describe "setting a font size"
    $ for_
      [ ("creates the font object and size field", "{}", "{\"font\":{\"size\":10}}")
      , ("adds the size field alongside other fields", "{\"font\":{\"family\":\"Mono\"}}", "{\"font\":{\"family\":\"Mono\",\"size\":10}}")
      , ("updates an existing size", "{\"font\":{\"size\":8}}", "{\"font\":{\"size\":10}}")
      ]
    $ \(name, input, expected) -> it name $ do
      output <- modifyFile input (jsonBytes . fontSize ?~ Number 10)
      decodeStrict @Value output `shouldBe` decodeStrict expected
  describe "YAML files" $ do
    it "creates a missing permissions object and appends to its write array" $ do
      output <- modifyFile "other: true\n" (yamlBytes . writePermissions %~ appendWorkspace)
      decodeYaml output
        `shouldBe` decodeStrict "{\"other\":true,\"permissions\":{\"write\":[\"workspace\"]}}"
    it "sets a nested field and keeps sibling fields" $ do
      output <- modifyFile "font:\n  family: Mono\n  size: 8\n" (yamlBytes . fontSize ?~ Number 10)
      decodeYaml output
        `shouldBe` decodeStrict "{\"font\":{\"family\":\"Mono\",\"size\":10}}"
    it "preserves comments and formatting when the value is unchanged" $ do
      let input = "# font settings\nfont: { size: 10 }\n"
      modifyFile input (yamlBytes . fontSize ?~ Number 10) >>= (`shouldBe` input)
    it "preserves invalid YAML" $ do
      let input = "font: [\n"
      modifyFile input (yamlBytes . fontSize ?~ Number 10) >>= (`shouldBe` input)
    it "preserves streams with more than one document" $ do
      let input = "font: { size: 8 }\n---\nother: true\n"
      modifyFile input (yamlBytes . fontSize ?~ Number 10) >>= (`shouldBe` input)
  describe "missing files" $ do
    it "creates a JSON file and its parent directory when adding a write permission" $
      drvTempDir $ \dir -> do
        let path = dir </> "config" </> "settings.json"
        drvModifyBinaryFile path (jsonBytes . writePermissions %~ appendWorkspace)
        output <- drvReadBinaryFile path
        decodeStrict @Value output `shouldBe` decodeStrict "{\"permissions\":{\"write\":[\"workspace\"]}}"
    it "creates a YAML file when setting a nested key" $
      drvTempDir $ \dir -> do
        let path = dir </> "settings.yaml"
        drvModifyBinaryFile path (yamlBytes . fontSize ?~ Number 10)
        output <- drvReadBinaryFile path
        decodeYaml output `shouldBe` decodeStrict "{\"font\":{\"size\":10}}"
    it "creates an empty JSON object when the edit leaves the default value unchanged" $
      drvTempDir $ \dir -> do
        let path = dir </> "settings.json"
        drvModifyBinaryFile path (jsonBytes . fontSize .~ Nothing)
        drvReadBinaryFile path >>= (`shouldBe` "{}")
