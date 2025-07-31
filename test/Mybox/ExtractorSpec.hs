module Mybox.ExtractorSpec where

import Codec.Archive.Zip qualified as Zip
import Codec.Compression.BZip qualified as BZip
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Lzma qualified as LZMA
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Mybox.Driver
import Mybox.Extractor
import Mybox.Package.Effects
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

temporaryZip :: Driver :> es => [Text] -> (Text -> Eff es a) -> Eff es a
temporaryZip paths act = drvTempDir $ \archiveDir -> do
  let addEntry path = Zip.addEntryToArchive $ Zip.toEntry (Text.unpack path) 0 mempty
  let archive = foldr addEntry Zip.emptyArchive paths

  let archiveFile = archiveDir </> "archive.zip"
  drvWriteBinaryFile archiveFile $ Zip.fromArchive archive

  act archiveFile

extractFileNames :: DIST es => Text -> Eff es (Set Text)
extractFileNames archive = do
  extractor <- getExtractor archive
  drvTempDir $ \dir -> do
    extract extractor archive dir
    files <- drvFind dir mempty{onlyFiles = True}
    pure $ Set.map (\f -> fromMaybe f $ Text.stripPrefix (dir <> "/") f) files

compress :: Driver :> es => (LBS.ByteString -> LBS.ByteString) -> Text -> Text -> (Text -> Eff es a) -> Eff es a
compress fn extension contents act = do
  drvTempDir $ \tempDir -> do
    let archive = tempDir </> "myfile." <> extension
    let compressed = fn $ LBS.fromStrict $ Text.encodeUtf8 contents
    drvWriteBinaryFile archive compressed
    act archive

spec :: Spec
spec =
  withTestEff $ withEff (nullTrackerSession . runInstallQueue) $ do
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
      for_ [(compress GZip.compress, "gz"), (compress LZMA.compress, "xz"), (compress BZip.compress, "bz2")] $
        \(compressAction, extension) ->
          it ("decompresses " <> Text.unpack extension) $
            compressAction extension "contents" $ \archive -> do
              extractor <- getRawExtractor archive
              drvTempFile $ \dest -> do
                extractRaw extractor archive dest
                extracted <- drvReadFile dest
                extracted `shouldBe` "contents"
