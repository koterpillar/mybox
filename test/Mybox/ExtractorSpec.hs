module Mybox.ExtractorSpec where

import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Extractor
import Mybox.SpecBase
import Mybox.Prelude

temporaryZip :: Driver :> es => [Text] -> (Text -> Eff es a) -> Eff es a
temporaryZip paths act = drvTempDir $ \srcDir ->
  drvTempDir $ \archiveDir -> do
    for_ paths $ \path -> do
      let fullPath = srcDir </> path
      drvMkdir $ pDirname fullPath
      drvWriteFile fullPath ""
 
    let archive = archiveDir </> "archive.zip"
    drvRun $ shellRaw $ "cd " <> shellQuote srcDir <> " && zip -qq -r " <> shellQuote archive <> " ."
 
    act archive

extractFileNames :: Driver :> es => Text -> Eff es (Set Text)
extractFileNames archive = do
  extractor <- getExtractor archive
  drvTempDir $ \dir -> do
    extract extractor archive dir
    files <- drvFind dir mempty{onlyFiles = True}
    pure $ Set.map (\f -> fromMaybe f $ Text.stripPrefix (dir <> "/") f) files

spec :: Spec
spec =
  around withTestEnv $ do
    it "unzips removing common prefix" $ do
      temporaryZip ["foo/bar", "foo/baz"] $ \archive -> do
        extractFileNames archive >>= (`shouldBe` Set.fromList ["bar", "baz"])
    it "stops upon multiple elements" $ do
      temporaryZip ["bar", "baz"] $ \archive -> do
        extractFileNames archive >>= (`shouldBe` Set.fromList ["bar", "baz"])
    it "stops when the only element is not a directory" $ do
      temporaryZip ["foo/bar"] $ \archive -> do
        extractFileNames archive >>= (`shouldBe` Set.fromList ["bar"])
    it "raises an error with too much nesting" $ do
      temporaryZip [Text.intercalate "/" $ replicate 100 "foo"] $ \archive -> do
        extractFileNames archive `shouldThrow` anyException
