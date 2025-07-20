module Mybox.Package.Archive (
  ArchiveReqs,
  parseArchive,
  archiveToJSON,
  ArchiveFields (..),
  ArchivePackage (..),
  archiveInstall,
) where

import Data.Aeson.Types (Pair)
import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Extractor
import Mybox.Package.Name
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

type ArchiveReqs p =
  ( PackageName p
  , HasField "raw" p (Either Text Bool)
  , HasField "binaries" p [Text]
  , HasField "binaryWrapper" p Bool
  , HasField "binaryPaths" p [Text]
  , HasField "apps" p [Text]
  , HasField "fonts" p [Text]
  )

data ArchiveFields = ArchiveFields
  { raw :: Either Text Bool
  , binaries :: [Text]
  , binaryWrapper :: Bool
  , binaryPaths :: [Text]
  , apps :: [Text]
  , fonts :: [Text]
  }

parseArchive :: Object -> Parser ArchiveFields
parseArchive o = do
  raw <- fromMaybe (Right False) <$> (fmap Left <$> o .:? "raw" <|> fmap Right <$> o .:? "raw")
  binaries <- parseCollapsedList o "binary" <|> pure []
  binaryWrapper <- o .:? "binary_wrapper" .!= False
  binaryPaths <- parseCollapsedList o "binary_path"
  apps <- parseCollapsedList o "app"
  fonts <- parseCollapsedList o "font"
  return ArchiveFields{..}

archiveToJSON :: ArchiveReqs p => p -> [Pair]
archiveToJSON p =
  [ "raw" .= either toJSON toJSON p.raw
  , "binary" .= p.binaries
  , "binary_wrapper" .= p.binaryWrapper
  , "binary_path" .= p.binaryPaths
  , "app" .= p.apps
  , "font" .= p.fonts
  ]

class ArchiveReqs p => ArchivePackage p where
  archiveUrl :: forall es. (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => p -> Eff es Text

pathname :: ArchivePackage p => p -> Text
pathname p = Text.replace "/" "--" p.name

aDirectory :: (ArchivePackage p, Driver :> es) => p -> Eff es Text
aDirectory p = do
  local <- drvLocal
  return $ local </> "mybox" </> pathname p

aExtract :: (ArchivePackage p, Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => p -> Text -> Text -> Eff es ()
aExtract p url archiveFile = case p.raw of
  Right True -> aExtractRaw p url archiveFile $ Text.takeWhileEnd (/= '/') url
  Left filename -> aExtractRaw p url archiveFile filename
  Right False -> do
    extractor <- getExtractor url
    target <- aDirectory p
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
    target <- aDirectory p
    let targetParent = pDirname target
    drvMkdir $ pDirname target
    trkAdd p targetParent
    trkAdd p target
    aExtract p url archiveFile
  for_ p.binaries $ installBinary p
  for_ p.apps $ installApp p
  for_ p.fonts $ installFont p

data AFindOptions = AFindOptions
  { paths :: [Text]
  , requireExecutable :: Bool
  , description :: Text
  }

aFind :: (ArchivePackage p, Driver :> es) => p -> AFindOptions -> Text -> Eff es Text
aFind p opt name = do
  directory <- aDirectory p
  paths <-
    filterM
      (if opt.requireExecutable then drvIsExecutable else drvIsFile)
      [ (if Text.null path then directory else directory </> path) </> name
      | path <- opt.paths
      ]
  case paths of
    (path : _) -> pure path
    [] -> terror $ "Cannot find " <> opt.description <> " '" <> name <> "' in " <> directory

binaryFind :: AFindOptions
binaryFind =
  AFindOptions
    { paths = ["", "bin"]
    , requireExecutable = True
    , description = "binary"
    }

aBinaryPath :: (ArchivePackage p, Driver :> es) => p -> Text -> Eff es Text
aBinaryPath p = aFind p binaryFind{paths = p.binaryPaths <> binaryFind.paths}

installBinary :: (ArchivePackage p, Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => p -> Text -> Eff es ()
installBinary p binary = do
  binaryPath <- aBinaryPath p binary
  local <- drvLocal
  let target = local </> "bin" </> binary
  if p.binaryWrapper
    then do
      drvWriteFile target $ Text.unlines ["#!/bin/sh", "exec " <> shellQuote binaryPath <> " \"$@\""]
      drvMakeExecutable target
    else
      drvLink binaryPath target
  trkAdd p target

freedesktopAppFind :: AFindOptions
freedesktopAppFind =
  AFindOptions
    { paths = ["share", "applications"]
    , requireExecutable = False
    , description = "application"
    }

installApp :: (ArchivePackage p, Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => p -> Text -> Eff es ()
installApp p app = do
  drvOS >>= \case
    Linux _ -> pure ()
    os -> error $ "Installing apps on " <> show os <> " is not implemented."
  let appDesktop = app <> ".desktop"
  appPath <- aFind p freedesktopAppFind appDesktop
  local <- drvLocal
  let appsTarget = local </> "share" </> "applications"
  let desktopTarget = appsTarget </> appDesktop
  drvLink appPath desktopTarget
  trkAdd p desktopTarget

-- FIXME: icon

fontExtensions :: [Text]
fontExtensions = ["ttf", "otf"]

installFont :: (ArchivePackage p, Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => p -> Text -> Eff es ()
installFont p font = do
  fontDir <-
    drvOS >>= \case
      Linux _ -> (\l -> l </> "share" </> "fonts") <$> drvLocal
      MacOS -> (\h -> h </> "Library" </> "Fonts") <$> drvHome
  directory <- aDirectory p
  fontPaths <- drvFind directory $ mempty{names = Just [font <> "." <> ext | ext <- fontExtensions]}
  fontPath <- case Set.toList fontPaths of
    [path] -> pure path
    [] -> terror $ "Cannot find font '" <> font <> "' in " <> directory
    _ -> terror $ "Multiple fonts found for '" <> font <> "' in " <> directory
  let targetPath = fontDir </> fontPath
  drvCopy fontPath targetPath
  trkAdd p targetPath
  queueInstall $ mkSystemPackage "fontconfig"
  drvRun $ "fc-cache" :| ["-f", fontDir]
