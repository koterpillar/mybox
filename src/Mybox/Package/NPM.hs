module Mybox.Package.NPM (NPMPackage (..), mkNPMPackage) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Tracker

data NPMPackage = NPMPackage
  { package :: Text
  , binaries :: [Text]
  , post :: [Text]
  }
  deriving (Eq, Show)

mkNPMPackage :: Text -> NPMPackage
mkNPMPackage package = NPMPackage{package, binaries = [], post = []}

instance HasField "name" NPMPackage Text where
  getField p = p.package

instance FromJSON NPMPackage where
  parseJSON = withObjectTotal "NPMPackage" $ do
    package <- takeField "npm"
    binaries <- takeCollapsedList "binary"
    post <- takePost
    pure NPMPackage{..}

instance ToJSON NPMPackage where
  toJSON p = object $ ["npm" .= p.package, "binary" .= p.binaries] <> postToJSON p

prerequisites :: App es => Eff es ()
prerequisites = do
  packages <- flip fmap drvOS $ \case
    Linux (Debian _) -> ["nodejs", "npm"]
    Linux Fedora -> ["nodejs", "nodejs-npm"]
    Linux (Generic d) -> terror $ "Cannot install npm on generic Linux: " <> d
    MacOS -> ["node"]
  for_ packages $ \package ->
    queueInstall $ mkSystemPackage package

viewVersion :: App es => NPMPackage -> Eff es Text
viewVersion p = do
  prerequisites
  drvRunOutput $ "npm" :| ["view", p.package, "version"]

npmInstall :: App es => NPMPackage -> Eff es ()
npmInstall p = do
  prerequisites

  let npmExec args = ("npm" :| ["exec", "--yes", "--package", p.package, "--"]) <> args

  -- Implicitly install the package
  drvRun $ npmExec ("true" :| [])

  -- Find the path where the binaries are installed
  npxPaths <- drvRunOutput $ npmExec $ shellRaw "echo $PATH"
  let npxBin' = find (elem "_npx" . (.segments)) $ map (mkPath @Abs) $ Text.splitOn ":" npxPaths

  case npxBin' of
    Nothing -> terror $ "Could not find npx path in " <> npxPaths
    Just npxBin -> do
      -- npxBin would be .../_npx/(hash)/node_modules/.bin, take the part up to
      -- and including the hash
      let npxPath = pParent npxBin >>= pParent
      for_ npxPath $ trkAdd p
      local <- drvLocal
      let binDir = local </> "bin"
      drvMkdir binDir

      forM_ p.binaries $ \name -> do
        let target = binDir </> name
        let script =
              Text.unlines
                [ "#!/bin/sh"
                , "PATH=" <> shellQuote npxBin.text <> ":$PATH"
                , "exec \"" <> name <> "\" \"$@\""
                ]
        drvWriteFile target script
        drvMakeExecutable target
        trkAdd p target

instance Package NPMPackage where
  localVersion = manualVersion
  remoteVersion = viewVersion
  install = installWithPost $ manualVersionInstall npmInstall
