module Mybox.Package.NPM.Internal where

import Data.Map qualified as Map
import Data.Set qualified as Set
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
import Mybox.Release
import Mybox.Tracker

data NPMPackage = NPMPackage
  { package :: Text
  , post :: [Text]
  }
  deriving (Eq, Generic, Show)

mkNPMPackage :: Text -> NPMPackage
mkNPMPackage package = NPMPackage{package, post = []}

instance PackageName NPMPackage where
  splitName = genericSplitName' @'[] @'["package"]

instance FromJSON NPMPackage where
  parseJSON = withObjectTotal "NPMPackage" $ do
    package <- takeField "npm"
    post <- takePost
    pure NPMPackage{..}

instance ToJSON NPMPackage where
  toJSON p = object $ ["npm" .= p.package] <> postToJSON p

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

newtype PackageJsonBin = PackageJsonBin {bin :: Either () (Set Text)} deriving (Eq, Show)

instance FromJSON PackageJsonBin where
  parseJSON = withObject "PackageJsonBin" $ \obj -> do
    binEither :: Maybe (CollapsedEither Text (Map Text Text)) <- obj .:? "bin"
    let bin = case fmap getCollapsedEither binEither of
          Just (Left _) -> Left ()
          Just (Right bins) -> Right $ Map.keysSet bins
          Nothing -> Right mempty
    pure $ PackageJsonBin{bin}

binariesFromPackageJson :: NPMPackage -> PackageJsonBin -> Set Text
binariesFromPackageJson p packageJson =
  either (const $ Set.singleton $ Text.takeWhileEnd (/= '/') p.package) id packageJson.bin

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
      npxPath <- maybe (throwString "Cannot find npx path") pure $ pParent npxBin >>= pParent
      trkAdd p npxPath
      local <- drvLocal False
      let binDir = local </> "bin"
      drvMkdir binDir

      packageJsonText <- drvReadFile (npxPath </> "node_modules" <//> mkPath @Rel p.package </> "package.json")
      packageJson <- jsonDecode @PackageJsonBin "package.json" packageJsonText
      let binaries = binariesFromPackageJson p packageJson

      forM_ binaries $ \name -> do
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

  -- FIXME: Return multiple npm releases, not just the latest version.
  releases p = fmap mkSingleRelease $ viewVersion p
  install = installWithPost $ manualVersionInstall $ \p _ -> npmInstall p
