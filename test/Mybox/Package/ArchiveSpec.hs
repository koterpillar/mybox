module Mybox.Package.ArchiveSpec where

import Mybox.Driver
import Mybox.Package.Archive.Internal
import Mybox.Prelude
import Mybox.SpecBase

data DummyPackage = DummyPackage {name :: Text, archive :: ArchiveFields}
  deriving (Eq, Ord, Show)

instance ArchivePackage DummyPackage where
  archiveUrl _ = pure "dummy"

spec :: Spec
spec = do
  describe "findBinary" $ do
    let pkg = DummyPackage{name = "name", archive = emptyArchiveFields}
    it "finds a binary in specified paths" $ do
      dir <- aDirectory pkg
      let bin = dir </> "bin" </> "test-binary"
      drvWriteFile bin ""
      drvMakeExecutable bin
      findBinary pkg "test-binary" >>= (`shouldBe` bin)
    it "errors when binary does not exist" $ do
      findBinary pkg "test-binary" `shouldThrow` anyErrorCall
  describe "withExtensions" $ do
    let extensions = ["txt", "md"]
    it "returns the same extension if already given" $
      withExtensions extensions "file.txt" `shouldBe` ["file.txt"]
    it "adds all extensions if not given" $
      withExtensions extensions "file" `shouldBe` ["file.txt", "file.md"]
    it "errors on multiple extensions" $ do
      evaluate (withExtensions extensions "file.txt.md") `shouldThrow` anyErrorCall
  describe "iconPath" $ do
    it "returns correct path for SVG" $
      iconPath (mkPath @AnyAnchor "icon.svg") `shouldBe` mkPath "hicolor/scalable/apps/icon.svg"
    it "returns correct path for PNG" $ do
      iconPath (mkPath @AnyAnchor "icon.png") `shouldBe` mkPath "hicolor/16x16/apps/icon.png"
    it "returns path with resolution for PNG" $ do
      iconPath (mkPath @AnyAnchor "somewhere/32x32/icon.png") `shouldBe` mkPath "hicolor/32x32/apps/icon.png"
