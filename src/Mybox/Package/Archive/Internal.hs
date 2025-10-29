module Mybox.Package.Archive.Internal where

import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Extractor
import Mybox.Package.Name
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Tracker

data ArchiveFields = ArchiveFields
  { raw :: Either Text Bool
  , binaries :: [Text]
  , binaryWrapper :: Bool
  , binaryPaths :: [Path Rel]
  , apps :: [Text]
  , fonts :: [Text]
  }
  deriving (Eq, Ord, Show)

emptyArchiveFields :: ArchiveFields
emptyArchiveFields =
  ArchiveFields
    { raw = Right False
    , binaries = []
    , binaryWrapper = False
    , binaryPaths = []
    , apps = []
    , fonts = []
    }

takeArchive :: ObjectParser ArchiveFields
takeArchive = do
  raw <- fromMaybe (Right False) . fmap getCollapsedEither <$> takeFieldMaybe "raw"
  binaries <- takeCollapsedList "binary"
  binaryWrapper <- fromMaybe False <$> takeFieldMaybe "binary_wrapper"
  binaryPaths <- takeCollapsedList "binary_path"
  apps <- takeCollapsedList "app"
  fonts <- takeCollapsedList "font"
  pure ArchiveFields{..}

archiveToJSON :: ArchiveFields -> [Pair]
archiveToJSON p =
  [ "raw" .= CollapsedEither p.raw
  , "binary" .= p.binaries
  , "binary_wrapper" .= p.binaryWrapper
  , "binary_path" .= p.binaryPaths
  , "app" .= p.apps
  , "font" .= p.fonts
  ]

class (HasField "archive" p ArchiveFields, PackageName p) => ArchivePackage p where
  archiveUrl :: forall es. App es => p -> Eff es Text

aDirectory :: (ArchivePackage p, Driver :> es) => p -> Eff es (Path Abs)
aDirectory p = do
  local <- drvLocal
  return $ local </> "mybox" </> pathname p

aExtract :: (App es, ArchivePackage p) => p -> Text -> Path Abs -> Eff es ()
aExtract p url archiveFile = case p.archive.raw of
  Right True -> aExtractRaw p url archiveFile $ Text.takeWhileEnd (/= '/') url
  Left filename -> aExtractRaw p url archiveFile filename
  Right False -> do
    extractor <- getExtractor url
    target <- aDirectory p
    extract extractor archiveFile target

aExtractRaw :: (App es, ArchivePackage p) => p -> Text -> Path Abs -> Text -> Eff es ()
aExtractRaw p url archiveFile filename = do
  extractor <- getRawExtractor url
  target <- aDirectory p
  drvMkdir target
  let targetPath = target </> filename
  extractRaw extractor archiveFile targetPath
  when (filename `elem` p.archive.binaries) $ drvMakeExecutable targetPath

archiveInstall :: (App es, ArchivePackage p) => p -> Eff es ()
archiveInstall p = do
  url <- archiveUrl p
  drvTempDownload url $ \archiveFile -> do
    target <- aDirectory p
    trkAdd p target
    aExtract p url archiveFile
  for_ p.archive.binaries $ installBinary p
  for_ p.archive.apps $ installApp p
  for_ p.archive.fonts $ installFont p

data AFindOptions = AFindOptions
  { paths :: [Path Rel]
  , requireExecutable :: Bool
  , description :: Text
  }

aFind :: (ArchivePackage p, Driver :> es) => p -> AFindOptions -> Text -> Eff es (Path Abs)
aFind p opt name = do
  directory <- aDirectory p
  paths <-
    filterM
      (if opt.requireExecutable then drvIsExecutable else drvIsFile)
      [directory <//> path </> name | path <- opt.paths]
  case paths of
    (path : _) -> pure path
    [] -> terror $ "Cannot find " <> opt.description <> " '" <> name <> "' in " <> directory.text

binaryFind :: AFindOptions
binaryFind =
  AFindOptions
    { paths = [pCurrent, mkPath "bin"]
    , requireExecutable = True
    , description = "binary"
    }

findBinary :: (ArchivePackage p, Driver :> es) => p -> Text -> Eff es (Path Abs)
findBinary p = aFind p binaryFind{paths = p.archive.binaryPaths <> binaryFind.paths}

installBinary :: (App es, ArchivePackage p) => p -> Text -> Eff es ()
installBinary p binary = do
  binaryPath <- findBinary p binary
  local <- drvLocal
  let target = local </> "bin" </> binary
  if p.archive.binaryWrapper
    then do
      drvWriteFile target $ Text.unlines ["#!/bin/sh", "exec " <> shellQuote binaryPath.text <> " \"$@\""]
      drvMakeExecutable target
    else
      drvLink binaryPath target
  trkAdd p target

freedesktopAppFind :: AFindOptions
freedesktopAppFind =
  AFindOptions
    { paths = ["share" </> "applications"]
    , requireExecutable = False
    , description = "application"
    }

installApp :: (App es, ArchivePackage p) => p -> Text -> Eff es ()
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
  appProperties <- parseDesktopFile <$> drvReadFile desktopTarget
  for_ (Map.lookup "Icon" appProperties) $ installIcon p

parseDesktopFile :: Text -> Map Text Text
parseDesktopFile contents = Map.fromList $ do
  line <- Text.lines contents
  guard $ not $ Text.null line
  guard $ line /= "[Desktop Entry]"
  [key, value] <- pure $ Text.splitOn "=" line
  pure (key, value)

withExtensions :: [Text] -> Text -> [Text]
withExtensions exts name = case Text.splitOn "." name of
  [base, ext] -> [base <> "." <> ext]
  [base] -> [base <> "." <> ext | ext <- exts]
  _ -> terror $ "Unexpected file name: " <> name <> " for extensions: " <> Text.intercalate ", " exts

iconExtensions :: [Text]
iconExtensions = ["png", "svg"]

installIcon :: (App es, ArchivePackage p) => p -> Text -> Eff es ()
installIcon p icon = do
  directory <- aDirectory p
  iconPaths <- drvFind directory $ findOptions{names = Just $ withExtensions iconExtensions icon}
  local <- drvLocal
  let iconsTarget = local </> "share" </> "icons"
  for_ iconPaths $ \iconSrcPath -> do
    let iconTargetPath = iconsTarget <//> iconPath iconSrcPath
    drvLink iconSrcPath iconTargetPath
    trkAdd p iconTargetPath

iconPath :: Anchor a => Path a -> Path Rel
iconPath icon = "hicolor" </> resolution </> "apps" </> base
 where
  parts = icon.segments
  base = icon.basename
  ext = Text.takeWhileEnd (/= '.') base
  resolution = case ext of
    "svg" -> "scalable"
    "png" -> fromMaybe "16x16" $ find isResolution parts
    _ -> terror $ "Unexpected icon extension: " <> ext <> " from " <> icon.text
  isResolution :: Text -> Bool
  isResolution p =
    case Text.splitOn "x" p of
      ns@[_, _] -> all (Text.all isDigit) ns
      _ -> False

fontExtensions :: [Text]
fontExtensions = ["ttf", "otf"]

installFont :: (App es, ArchivePackage p) => p -> Text -> Eff es ()
installFont p font = do
  fontDir <-
    drvOS >>= \case
      Linux _ -> (\l -> l </> "share" </> "fonts") <$> drvLocal
      MacOS -> (\h -> h </> "Library" </> "Fonts") <$> drvHome
  directory <- aDirectory p
  fontPaths <- drvFind directory $ findOptions{names = Just $ withExtensions fontExtensions font}
  fontPath <- case Set.toList fontPaths of
    [path] -> pure path
    [] -> terror $ "Cannot find font '" <> font <> "' in " <> directory.text
    _ -> terror $ "Multiple fonts found for '" <> font <> "' in " <> directory.text
  let targetPath = fontDir </> fontPath.basename
  drvCopy fontPath targetPath
  trkAdd p targetPath
  queueInstall $ mkSystemPackage "fontconfig"
  drvRun $ "fc-cache" :| ["-f", fontDir.text]
