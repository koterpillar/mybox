module Mybox.ExtractorSpec where

import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Extractor
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores
import Mybox.Tracker

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

extractFileNames :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => Text -> Eff es (Set Text)
extractFileNames archive = do
  extractor <- getExtractor archive
  drvTempDir $ \dir -> do
    extract extractor archive dir
    files <- drvFind dir mempty{onlyFiles = True}
    pure $ Set.map (\f -> fromMaybe f $ Text.stripPrefix (dir <> "/") f) files

prerequisites :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => Eff es ()
prerequisites = do
  -- FIXME: Cannot test prerequisites of the actual extractors
  ensureInstalled $ mkSystemPackage "zip"
  xz <- flip fmap drvOS $ \case
    Linux Fedora -> "xz"
    Linux (Debian _) -> "xz-utils"
    MacOS -> "xz"
  ensureInstalled $ mkSystemPackage xz
  ensureInstalled $ mkSystemPackage "bzip2"

spec :: Spec
spec =
  around (withTestEnvAnd $ nullTrackerSession . runInstallQueue) $
    beforeAll_ prerequisites $ do
      describe "extract" $ do
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
      describe "getExtractor" $ do
        it "guesses extractor from a link" $ do
          extractor <- getExtractor "http://example.com/test.tar.gz"
          show extractor `shouldBe` "tar -z"
        it "guesses extractor from a link redirect" $ do
          extractor <- getExtractor "https://telegram.org/dl/desktop/linux"
          show extractor `shouldBe` "tar -J"
        it "raises an error with unknown format" $ do
          getExtractor "https://example.com" `shouldThrow` anyException
      describe "RawExtractor" $
        for_ [("gzip", "gz"), ("xz", "xz"), ("bzip2", "bz2")] $
          \(compressCommand :: Text, extension :: Text) ->
            it ("decompresses " <> show compressCommand) $ do
              drvTempDir $ \tempDir -> do
                let srcFile = tempDir </> "myfile"
                drvWriteFile srcFile "contents"
                drvRun $ compressCommand :| [srcFile]
                let archive = tempDir </> "myfile." <> extension
                extractor <- getRawExtractor archive
                drvTempFile $ \dest -> do
                  extractRaw extractor archive dest
                  extracted <- drvReadFile dest
                  extracted `shouldBe` "contents"
