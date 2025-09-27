module Mybox.Extractor (
  Extractor,
  RawExtractor,
  getExtractor,
  getRawExtractor,
  extract,
  extractRaw,
) where

import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.System
import Mybox.Path
import Mybox.Prelude

data Extractor = Extractor
  { extractExact :: forall es a. (Anchor a, App es) => Path a -> Path Abs -> Eff es ()
  , description :: Text
  }

instance Show Extractor where
  show Extractor{description} = Text.unpack description

findContents :: Driver :> es => Path Abs -> Int -> Eff es (Path Abs, Set (Path Rel))
findContents sourceDir maxDepth = do
  if maxDepth < 0
    then terror $ "maxDepth must be positive, got " <> Text.pack (show maxDepth)
    else do
      contents <- drvFind sourceDir (mempty{maxDepth = Just 1})
      case Set.toList contents of
        [element] -> do
          isDir <- drvIsDir element
          if isDir
            then findContents element (pred maxDepth)
            else pure (sourceDir, Set.map (pRelativeTo_ sourceDir) contents)
        _ -> pure (sourceDir, Set.map (pRelativeTo_ sourceDir) contents)

extract :: (Anchor a1, Anchor a2, App es) => Extractor -> Path a1 -> Path a2 -> Eff es ()
extract extractor archive targetDirectory = drvTempDir $ \tmpdir -> do
  extractExact extractor archive tmpdir
  (contentsDir, contents) <- findContents tmpdir 10
  for_ contents $ \element -> do
    let target = pWiden targetDirectory <//> element
    drvCopy (contentsDir <//> element) target

tar_ :: Maybe Text -> (forall es. App es => Eff es ()) -> Extractor
tar_ option ensure = Extractor{extractExact = extractTar, description = Text.unwords $ "tar" : toList option}
 where
  extractTar :: (Anchor a, App es) => Path a -> Path Abs -> Eff es ()
  extractTar archive targetDirectory = do
    ensure
    tarCmd <- drvFindExecutable ["gtar", "tar"]
    drvRun $ tarCmd :| ["--extract", "--directory", targetDirectory.text] ++ toList option ++ ["--file", archive.text]

tar :: Extractor
tar = tar_ Nothing (pure ())

tarGz :: Extractor
tarGz = tar_ (Just "-z") (pure ())

tarBz2 :: Extractor
tarBz2 = tar_ (Just "-j") ensureBunzip2

tarXz :: Extractor
tarXz = tar_ (Just "-J") ensureXz

unzipE :: Extractor
unzipE = Extractor{extractExact = extractUnzip, description = "unzip"}
 where
  extractUnzip :: (Anchor a, App es) => Path a -> Path Abs -> Eff es ()
  extractUnzip archive targetDirectory = do
    unlessExecutableExists "unzip" $ ensureInstalled (mkSystemPackage "unzip")
    drvRun $ "unzip" :| ["-o", "-qq", archive.text, "-d", targetDirectory.text]

withRedirect :: Driver :> es => (Text -> Maybe a) -> Eff es a -> Text -> Eff es a
withRedirect f fallback url =
  f url `fromMaybeOrM` (f <$> drvRedirectLocation url) `fromMaybeOrMM` fallback

guessExtractor :: Text -> Maybe Extractor
guessExtractor url = go
 where
  go
    | hasSuffix ".tar" = Just tar
    | hasSuffix ".tar.gz" || hasSuffix ".tgz" = Just tarGz
    | hasSuffix ".tar.bz2" = Just tarBz2
    | hasSuffix ".tar.xz" || hasSuffix ".txz" = Just tarXz
    | hasSuffix ".zip" = Just unzipE
    | otherwise = Nothing
  hasSuffix suffix = Text.isSuffixOf suffix url

getExtractor :: Driver :> es => Text -> Eff es Extractor
getExtractor = withRedirect guessExtractor $ terror "Unknown archive format"

data RawExtractor = RawExtractor
  { extractRaw_ :: forall es a1 a2. (Anchor a1, Anchor a2, App es) => Path a1 -> Path a2 -> Eff es ()
  , description :: Text
  }

instance Show RawExtractor where
  show RawExtractor{description} = Text.unpack description

mkRawExtractor :: Text -> (forall es a1 a2. (Anchor a1, Anchor a2, App es) => Path a1 -> Path a2 -> Eff es ()) -> RawExtractor
mkRawExtractor description extractRaw_ = RawExtractor{extractRaw_ = extractRaw_, description}

extractRaw :: (Anchor a1, Anchor a2, App es) => RawExtractor -> Path a1 -> Path a2 -> Eff es ()
extractRaw e = extractRaw_ e

pipeCommand :: (Anchor a1, Anchor a2, App es) => Text -> Path a1 -> Path a2 -> Eff es ()
pipeCommand command archive target = do
  drvRun $ shellRaw $ command <> " < " <> shellQuote archive.text <> " > " <> shellQuote target.text

gunzip :: RawExtractor
gunzip = mkRawExtractor "gunzip" $ pipeCommand "gunzip"

ensureXz :: App es => Eff es ()
ensureXz = unlessExecutableExists "xzcat" $ do
  prerequisite <- flip fmap drvOS $ \case
    Linux Fedora -> "xz"
    Linux (Debian _) -> "xz-utils"
    MacOS -> "xz"
  ensureInstalled $ mkSystemPackage prerequisite

xz :: RawExtractor
xz = mkRawExtractor "xz" $ \archive target -> do
  ensureXz
  pipeCommand "xzcat" archive target

ensureBunzip2 :: App es => Eff es ()
ensureBunzip2 = unlessExecutableExists "bunzip2" $ do
  prerequisite <- flip fmap drvOS $ \case
    Linux _ -> Just "bzip2"
    MacOS -> Nothing
  forM_ prerequisite $ ensureInstalled . mkSystemPackage

bunzip2 :: RawExtractor
bunzip2 = mkRawExtractor "bunzip2" $ \archive target -> do
  ensureBunzip2
  pipeCommand "bunzip2" archive target

move :: RawExtractor
move = RawExtractor{extractRaw_ = drvCopy, description = "move"}

guessRawExtractor :: Text -> Maybe RawExtractor
guessRawExtractor url = go
 where
  go
    | hasSuffix ".gz" = Just gunzip
    | hasSuffix ".xz" = Just xz
    | hasSuffix ".bz2" = Just bunzip2
    | otherwise = Nothing
  hasSuffix suffix = Text.isSuffixOf suffix url

getRawExtractor :: Driver :> es => Text -> Eff es RawExtractor
getRawExtractor = withRedirect guessRawExtractor $ pure move

unlessExecutableExists :: Driver :> es => Text -> Eff es () -> Eff es ()
unlessExecutableExists command act = drvExecutableExists command >>= (`unless` act)
