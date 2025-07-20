module Mybox.Package.Archive (
  ArchiveReqs,
  parseArchive,
  ArchivePackage (..),
  archiveInstall,
) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Extractor
import Mybox.Package.Name
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

type ArchiveReqs p = (PackageName p, HasField "raw" p (Either Text Bool), HasField "binaries" p [Text])

parseArchive :: Object -> Parser (Either String Bool, [Text])
parseArchive o = do
  raw <- fromMaybe (Right False) <$> (fmap Left <$> o .:? "raw" <|> fmap Right <$> o .:? "raw")
  binaries <- parseCollapsedList o "binary"
  return (raw, binaries)

class ArchiveReqs p => ArchivePackage p where
  archiveUrl :: forall es. (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => p -> Eff es Text

pathname :: ArchivePackage p => p -> Text
pathname p = Text.replace "/" "--" p.name

directory :: (ArchivePackage p, Driver :> es) => p -> Eff es Text
directory p = do
  local <- drvLocal
  return $ local </> "mybox" </> pathname p

aExtract :: (ArchivePackage p, Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => p -> Text -> Text -> Eff es ()
aExtract p url archiveFile = case p.raw of
  Right True -> aExtractRaw p url archiveFile $ Text.takeWhileEnd (/= '/') url
  Left filename -> aExtractRaw p url archiveFile filename
  Right False -> do
    extractor <- getExtractor url
    target <- directory p
    extract extractor archiveFile target

aExtractRaw :: (ArchivePackage p, Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => p -> Text -> Text -> Text -> Eff es ()
aExtractRaw p url archiveFile filename = do
  extractor <- getRawExtractor url
  extractRaw extractor archiveFile filename
  when (filename `elem` p.binaries) $ drvMakeExecutable filename

archiveInstall :: (ArchivePackage p, Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => p -> Eff es ()
archiveInstall p = do
  url <- archiveUrl p
  drvTempFile $ \archiveFile -> do
    drvRun $ "curl" :| ["-fsSL", "-o", archiveFile, url]
    target <- directory p
    let targetParent = pDirname target
    drvMkdir $ pDirname target
    trkAdd p targetParent
    trkAdd p target
    aExtract p url archiveFile
