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
