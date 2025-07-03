module Mybox.Package.NPM (
  NPMPackage (..),
) where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Prelude
import Mybox.Tracker

data NPMPackage = NPMPackage
  { package :: Text
  , binaries :: [Text]
  }
  deriving (Eq, Generic, Show)

instance HasField "name" NPMPackage Text where
  getField p = p.package

instance FromJSON NPMPackage

instance ToJSON NPMPackage

viewVersion :: Driver :> es => NPMPackage -> Eff es Text
viewVersion p = drvRunOutput $ "npm" :| ["view", p.package, "version"]

npmInstall :: (Driver :> es, PackageTracker :> es) => NPMPackage -> Eff es ()
npmInstall p = do
  let npmExec args = ("npm" :| ["exec", "--yes", "--package", p.package, "--"]) <> args

  -- Implicitly install the package
  drvRun $ npmExec ("true" :| [])

  -- Find the path where the binaries are installed
  npxPaths <- drvRunOutput $ npmExec $ shellRaw "echo $PATH"
  let npxPath = find (Text.isInfixOf "_npx") $ Text.splitOn ":" npxPaths

  case npxPath of
    Nothing -> error $ "Could not find npx path in " <> Text.unpack npxPaths
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
        trkAdd target

instance Package NPMPackage where
  localVersion = manualVersion
  remoteVersion = viewVersion
  install = manualVersionInstall npmInstall
