module Mybox.Package.Archive (
  ArchiveReqs,
  parseArchive,
  archiveToJSON,
  ArchiveFields (..),
  ArchivePackage (..),
  archiveInstall,
  parseDesktopFile,
) where

import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Extractor
import Mybox.Package.Effects
import Mybox.Package.Name
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Tracker

type ArchiveReqs p =
  ( PackageName p
  , HasField "raw" p (Either Text Bool)
  , HasField "binaries" p [Text]
  , HasField "binaryWrapper" p Bool
  , HasField "binaryPaths" p [Path Rel]
  , HasField "apps" p [Text]
  , HasField "fonts" p [Text]
  )

data ArchiveFields = ArchiveFields
  { raw :: Either Text Bool
  , binaries :: [Text]
  , binaryWrapper :: Bool
  , binaryPaths :: [Path Rel]
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
  archiveUrl :: forall es. DIST es => p -> Eff es Text

pathname :: ArchivePackage p => p -> Text
pathname p = Text.replace "/" "--" p.name

aDirectory :: (ArchivePackage p, Driver :> es) => p -> Eff es (Path Abs)
aDirectory p = do
  local <- drvLocal
  return $ local </> "mybox" </> pathname p

aExtract :: (ArchivePackage p, DIST es) => p -> Text -> Path Abs -> Eff es ()
aExtract p url archiveFile = case p.raw of
  Right True -> aExtractRaw p url archiveFile $ Text.takeWhileEnd (/= '/') url
  Left filename -> aExtractRaw p url archiveFile filename
  Right False -> do
    extractor <- getExtractor url
    target <- aDirectory p
    extract extractor archiveFile target

aExtractRaw :: (ArchivePackage p, DIST es) => p -> Text -> Path Abs -> Text -> Eff es ()
aExtractRaw p url archiveFile filename = do
  extractor <- getRawExtractor url
  target <- aDirectory p
  let targetPath = target </> filename
  extractRaw extractor archiveFile targetPath
  when (filename `elem` p.binaries) $ drvMakeExecutable targetPath

archiveInstall :: (ArchivePackage p, DIST es) => p -> Eff es ()
archiveInstall p = do
  url <- archiveUrl p
  drvTempFile $ \archiveFile -> do
    drvRun $ "curl" :| ["-fsSL", "-o", archiveFile.text, url]
    target <- aDirectory p
    drvMkdir target.dirname
    trkAdd p target.dirname
    trkAdd p target
    aExtract p url archiveFile
  for_ p.binaries $ installBinary p
  for_ p.apps $ installApp p
  for_ p.fonts $ installFont p

data AFindOptions = AFindOptions
  { paths :: [Maybe (Path Rel)]
  , requireExecutable :: Bool
  , description :: Text
  }

aFind :: (ArchivePackage p, Driver :> es) => p -> AFindOptions -> Text -> Eff es (Path Abs)
aFind p opt name = do
  directory <- aDirectory p
  paths <-
    filterM
      (if opt.requireExecutable then drvIsExecutable else drvIsFile)
      [ (case path of Nothing -> directory; Just path' -> directory <//> path') </> name
      | path <- opt.paths
      ]
  case paths of
    (path : _) -> pure path
    [] -> terror $ "Cannot find " <> opt.description <> " '" <> name <> "' in " <> directory.text

binaryFind :: AFindOptions
binaryFind =
  AFindOptions
    { paths = [Nothing, Just $ mkPath "bin"]
    , requireExecutable = True
    , description = "binary"
    }

aBinaryPath :: (ArchivePackage p, Driver :> es) => p -> Text -> Eff es (Path Abs)
aBinaryPath p = aFind p binaryFind{paths = map Just p.binaryPaths <> binaryFind.paths}

installBinary :: (ArchivePackage p, DIST es) => p -> Text -> Eff es ()
installBinary p binary = do
  binaryPath <- aBinaryPath p binary
  local <- drvLocal
  let target = local </> "bin" </> binary
  if p.binaryWrapper
    then do
      drvWriteFile target $ Text.unlines ["#!/bin/sh", "exec " <> shellQuote binaryPath.text <> " \"$@\""]
      drvMakeExecutable target
    else
      drvLink binaryPath target
  trkAdd p target

freedesktopAppFind :: AFindOptions
freedesktopAppFind =
  AFindOptions
    { paths = [Just ("share" </> "applications")]
    , requireExecutable = False
    , description = "application"
    }

installApp :: (ArchivePackage p, DIST es) => p -> Text -> Eff es ()
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

installIcon :: (ArchivePackage p, DIST es) => p -> Text -> Eff es ()
installIcon p icon = do
  directory <- aDirectory p
  iconPaths <- drvFind directory $ mempty{names = Just $ withExtensions iconExtensions icon}
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

installFont :: (ArchivePackage p, DIST es) => p -> Text -> Eff es ()
installFont p font = do
  fontDir <-
    drvOS >>= \case
      Linux _ -> (\l -> l </> "share" </> "fonts") <$> drvLocal
      MacOS -> (\h -> h </> "Library" </> "Fonts") <$> drvHome
  directory <- aDirectory p
  fontPaths <- drvFind directory $ mempty{names = Just $ withExtensions fontExtensions font}
  fontPath <- case Set.toList fontPaths of
    [path] -> pure path
    [] -> terror $ "Cannot find font '" <> font <> "' in " <> directory.text
    _ -> terror $ "Multiple fonts found for '" <> font <> "' in " <> directory.text
  let targetPath = fontDir </> fontPath.basename
  drvCopy fontPath targetPath
  trkAdd p targetPath
  queueInstall $ mkSystemPackage "fontconfig"
  drvRun $ "fc-cache" :| ["-f", fontDir.text]
