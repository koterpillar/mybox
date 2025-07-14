module Mybox.Extractor (
  Extractor (..),
  RawExtractor (..),
  getExtractor,
  getRawExtractor,
  extract,
) where

import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Path
import Mybox.Prelude

newtype Extractor = Extractor
  { extractExact :: forall es. Driver :> es => Text -> Text -> Eff es ()
  }

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
            then findContents (sourceDir </> element) (pred maxDepth)
            else pure contents
        _ -> pure contents

extract :: Driver :> es => Extractor -> Text -> Text -> Eff es ()
extract extractor archive targetDirectory = do
  tmpdir <- drvTempDir
  extractExact extractor archive tmpdir
  contents <- findContents tmpdir 10
  for_ contents $ \element -> drvCopy element (targetDirectory </> fromJust (pFilename element))

tar :: [Text] -> Extractor
tar options = Extractor{extractExact = extractTar}
 where
  extractTar :: Driver :> es => Text -> Text -> Eff es ()
  extractTar archive targetDirectory = do
    tarCmd <- drvFindExecutable ["gtar", "tar"]
    drvRun $ tarCmd :| ("--extract" : "--directory" : targetDirectory : options ++ ["--file", archive])

unzipE :: Extractor
unzipE = Extractor{extractExact = extractUnzip}
 where
  extractUnzip :: Driver :> es => Text -> Text -> Eff es ()
  extractUnzip archive targetDirectory = do
    drvRun $ "unzip" :| ["-o", "-qq", archive, "-d", targetDirectory]

withRedirect :: Driver :> es => (Text -> Maybe a) -> Eff es a -> Text -> Eff es a
withRedirect f fallback url = do
  case f url of
    Just a -> pure a
    Nothing -> do
      redirectUrl <- drvRedirectLocation url
      case f redirectUrl of
        Just a -> pure a
        Nothing -> fallback

guessExtractor :: Text -> Maybe Extractor
guessExtractor url = go
 where
  go
    | hasSuffix ".tar" = Just $ tar []
    | hasSuffix ".tar.gz" || hasSuffix ".tgz" = Just $ tar ["-z"]
    | hasSuffix ".tar.bz2" = Just $ tar ["-j"]
    | hasSuffix ".tar.xz" || hasSuffix ".txz" = Just $ tar ["-J"]
    | hasSuffix ".zip" = Just unzipE
    | otherwise = Nothing
  hasSuffix suffix = Text.isSuffixOf suffix url

getExtractor :: Driver :> es => Text -> Eff es Extractor
getExtractor = withRedirect guessExtractor $ terror "Unknown archive format"

newtype RawExtractor = RawExtractor
  { extractRaw :: forall es. Driver :> es => Text -> Text -> Eff es ()
  }

pipeE :: Text -> RawExtractor
pipeE command = RawExtractor{extractRaw = pipeExtract}
 where
  pipeExtract :: Driver :> es => Text -> Text -> Eff es ()
  pipeExtract archive target =
    drvRun $ shellRaw $ command <> " < " <> shellQuote archive <> " > " <> shellQuote target

moveE :: RawExtractor
moveE = RawExtractor{extractRaw = drvCopy}

guessRawExtractor :: Text -> Maybe RawExtractor
guessRawExtractor url = go
 where
  go
    | hasSuffix ".gz" = Just $ pipeE "gunzip"
    | hasSuffix ".xz" = Just $ pipeE "xzcat"
    | hasSuffix ".bz2" = Just $ pipeE "bunzip2"
    | otherwise = Nothing
  hasSuffix suffix = Text.isSuffixOf suffix url

getRawExtractor :: Driver :> es => Text -> Eff es RawExtractor
getRawExtractor = withRedirect guessRawExtractor $ pure moveE
