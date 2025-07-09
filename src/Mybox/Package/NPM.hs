module Mybox.Package.NPM (
  NPMPackage (..),
) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

data NPMPackage = NPMPackage
  { package :: Text
  , binaries :: [Text]
  }
  deriving (Eq, Show)

instance HasField "name" NPMPackage Text where
  getField p = p.package

instance FromJSON NPMPackage where
  parseJSON = withObject "NPMPackage" $ \o -> do
    package <- o .: "npm"
    binaries <- parseCollapsedList o "binary"
    pure NPMPackage{..}

instance ToJSON NPMPackage where
  toJSON p = object ["npm" .= p.package, "binary" .= p.binaries]

prerequisites :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => Eff es ()
prerequisites = do
  os <- drvOS
  let packages = case os of
        Linux d ->
          "nodejs" : case d of
            Debian _ -> ["npm"]
            Fedora -> ["nodejs-npm"]
        MacOS -> ["node"]
  for_ packages $ \package ->
    queueInstall $ SystemPackage{name = package, url = Nothing, autoUpdates = False}

viewVersion :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => NPMPackage -> Eff es Text
viewVersion p = do
  prerequisites
  drvRunOutput $ "npm" :| ["view", p.package, "version"]

npmInstall :: (Driver :> es, InstallQueue :> es, Stores :> es, TrackerSession :> es) => NPMPackage -> Eff es ()
npmInstall p = do
  prerequisites

  let npmExec args = ("npm" :| ["exec", "--yes", "--package", p.package, "--"]) <> args

  -- Implicitly install the package
  drvRun $ npmExec ("true" :| [])

  -- Find the path where the binaries are installed
  npxPaths <- drvRunOutput $ npmExec $ shellRaw "echo $PATH"
  let npxPath = find (Text.isInfixOf "_npx") $ Text.splitOn ":" npxPaths

  case npxPath of
    Nothing -> terror $ "Could not find npx path in " <> npxPaths
    Just path -> do
      local <- drvLocal
      let binDir = local </> "bin"
      drvMkdir binDir

      forM_ p.binaries $ \name -> do
        let target = binDir </> name
        let script =
              Text.unlines
                [ "#!/bin/sh"
                , "PATH=" <> shellQuote path <> ":$PATH"
                , "exec \"" <> name <> "\" \"$@\""
                ]
        drvWriteFile target script
        drvMakeExecutable target
        trkAdd p target

instance Package NPMPackage where
  localVersion = manualVersion
  remoteVersion = viewVersion
  install = manualVersionInstall npmInstall
