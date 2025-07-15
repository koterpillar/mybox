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
import Mybox.Package.Class
import Mybox.Package.Queue.Effect
import Mybox.Package.System
import Mybox.Path
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

data Extractor = Extractor
  { extractExact :: forall es. (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => Text -> Text -> Eff es ()
  , description :: Text
  }

instance Show Extractor where
  show Extractor{description} = Text.unpack description

findContents :: Driver :> es => Text -> Int -> Eff es (Set Text)
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
            else pure contents
        _ -> pure contents

extract :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => Extractor -> Text -> Text -> Eff es ()
extract extractor archive targetDirectory = drvTempDir $ \tmpdir -> do
  extractExact extractor archive tmpdir
  contents <- findContents tmpdir 10
  for_ contents $ \element -> drvCopy element (targetDirectory </> fromJust (pFilename element))

tar_ :: Maybe Text -> Extractor
tar_ option = Extractor{extractExact = extractTar, description = Text.unwords $ "tar" : toList option}
 where
  extractTar :: Driver :> es => Text -> Text -> Eff es ()
  extractTar archive targetDirectory = do
    tarCmd <- drvFindExecutable ["gtar", "tar"]
    drvRun $ tarCmd :| ["--extract", "--directory", targetDirectory] ++ toList option ++ ["--file", archive]

tar :: Extractor
tar = tar_ Nothing

tarGz :: Extractor
tarGz = tar_ (Just "-z")

tarBz2 :: Extractor
tarBz2 = tar_ (Just "-j")

tarXz :: Extractor
tarXz = tar_ (Just "-J")

unzipE :: Extractor
unzipE = Extractor{extractExact = extractUnzip, description = "unzip"}
 where
  extractUnzip :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => Text -> Text -> Eff es ()
  extractUnzip archive targetDirectory = do
    unlessExecutableExists "unzip" $ ensureInstalled (mkSystemPackage "unzip")
    drvRun $ "unzip" :| ["-o", "-qq", archive, "-d", targetDirectory]

withRedirect :: Driver :> es => (Text -> Maybe a) -> Eff es a -> Text -> Eff es a
withRedirect f fallback url = do
  case f url of
    Just a -> pure a
    Nothing -> do
      redirectUrl <- drvRedirectLocation url
      maybe fallback pure $ f redirectUrl

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
  { extractRaw_ :: forall es. (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => Text -> Text -> Eff es ()
  , description :: Text
  }

mkRawExtractor :: Text -> (forall es. (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => Text -> Text -> Eff es ()) -> RawExtractor
mkRawExtractor description extractRaw_ = RawExtractor{extractRaw_ = extractRaw_, description}

extractRaw :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => RawExtractor -> Text -> Text -> Eff es ()
extractRaw e = extractRaw_ e

pipeCommand :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => Text -> Text -> Text -> Eff es ()
pipeCommand command archive target = do
  drvRun $ shellRaw $ command <> " < " <> shellQuote archive <> " > " <> shellQuote target

gunzip :: RawExtractor
gunzip = mkRawExtractor "gunzip" $ pipeCommand "gunzip"

xz :: RawExtractor
xz = mkRawExtractor "xz" $ \archive target -> do
  unlessExecutableExists "xzcat" $ do
    prerequisite <- flip fmap drvOS $ \case
      Linux Fedora -> "xz"
      Linux (Debian _) -> "xz-utils"
      MacOS -> "xz"
    ensureInstalled $ mkSystemPackage prerequisite
  pipeCommand "xzcat" archive target

bunzip2 :: RawExtractor
bunzip2 = mkRawExtractor "bunzip2" $ \archive target -> do
  unlessExecutableExists "bunzip2" $ do
    prerequisite <- flip fmap drvOS $ \case
      Linux _ -> Just "bzip2"
      MacOS -> Nothing
    forM_ prerequisite $ ensureInstalled . mkSystemPackage
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
