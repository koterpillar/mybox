module Mybox.Package.GithubSpec where

import Control.Monad.Extra (anyM)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Package.Archive
import Mybox.Package.Github
import Mybox.Package.SpecBase
import Mybox.Package.System
import Mybox.Prelude
import Mybox.SpecBase

-- | Assert that a desktop file exists with the given name and optional executable
assertDesktopFileExists ::
  (Driver :> es, IOE :> es) =>
  Text -> Text -> Maybe Text -> Eff es ()
assertDesktopFileExists fileName expectedName expectedExecutable = do
  os <- drvOS
  case os of
    Linux _ -> do
      local <- drvLocal
      let desktopFilePath = local </> "share" </> "applications" </> pSegment (fileName <> ".desktop")
      desktopContent <- parseDesktopFile <$> drvReadFile desktopFilePath

      Map.lookup "Name" desktopContent `shouldBe` Just expectedName

      for_ expectedExecutable $ \exec ->
        case Map.lookup "Exec" desktopContent of
          Just command -> command `shouldContainText` exec
          Nothing -> expectationFailure "Exec field not found in desktop file"

      for_ (Map.lookup "Icon" desktopContent) assertIconExists
    _ -> pure ()

pngResolutions :: [Text]
pngResolutions = do
  p :: Int <- [4 .. 8]
  let res :: Int = 2 ^ p
  let resStr = Text.pack $ show res
  pure $ resStr <> "x" <> resStr

iconPaths :: Text -> [Path]
iconPaths name = do
  (base, ext) <- case Text.splitOn "." name of
    [base_, ext_] -> pure (base_, ext_)
    [base_] -> (base_,) <$> ["png", "svg"]
    _ -> terror $ "Unexpected icon name: " <> name
  size <- case ext of
    "png" -> pngResolutions
    "svg" -> ["scalable"]
    _ -> terror $ "Unexpected icon extension: " <> ext
  pure $ "hicolor" </> pSegment size </> "apps" </> pSegment (base <> "." <> ext)

assertIconExists ::
  forall es.
  (Driver :> es, IOE :> es) =>
  Text ->
  Eff es ()
assertIconExists iconName = do
  os <- drvOS
  case os of
    Linux _ -> do
      local <- drvLocal
      let iconsDir = local </> "share" </> "icons"

      drvIsDir iconsDir >>= (`shouldBe` True)

      iconExists <- anyM (\p -> drvIsFile (iconsDir </> p)) (iconPaths iconName)
      unless iconExists $ do
        allFiles <- drvFind iconsDir (mempty{onlyFiles = True})
        expectationFailure $
          "Icon '"
            <> Text.unpack iconName
            <> "' not found. Files in icons directory: "
            <> show allFiles
    _ -> pure ()

spec :: Spec
spec = do
  jsonSpec (Nothing @GithubPackage) [(Nothing, "{\"repo\": \"example/example\"}")]

  skipIf ((== Aarch64) <$> drvArchitecture) $
    packageSpec $ \psa ->
      ps
        ( (mkGithubPackage "neovim/neovim")
            { Mybox.Package.Github.binaries = ["nvim"]
            , Mybox.Package.Github.apps = case psa.os of
                Linux _ -> ["nvim"]
                _ -> []
            }
        )
        & checkInstalled
          ( do
              commandHasOutput ("nvim" :| ["--version"]) "NVIM"
              case psa.os of
                Linux _ -> do
                  assertDesktopFileExists "nvim" "Neovim" (Just "nvim")
                _ -> pure ()
          )

  -- Eza does not provide macOS binaries
  onlyIfOS (\case Linux _ -> True; _ -> False) $
    packageSpec $ \_ ->
      ps
        ( (mkGithubPackage "eza-community/eza")
            { Mybox.Package.Github.binaries = ["eza"]
            , Mybox.Package.Github.excludes = [".zip", "no_libgit"]
            }
        )
        & checkInstalledCommandOutput ("eza" :| ["--version"]) "eza - A modern, maintained replacement for ls"

  packageSpec $ \psa ->
    ps
      ( (mkGithubPackage "com-lihaoyi/Ammonite")
          { Mybox.Package.Github.binaries = ["amm"]
          , Mybox.Package.Github.prefixes = ["3.6-"]
          , Mybox.Package.Github.suffixes = ["-bootstrap"]
          , Mybox.Package.Github.raw = Left "amm"
          }
      )
      & ( case psa.os of
            Linux Fedora -> preinstallPackage $ mkSystemPackage "java-21-openjdk"
            Linux (Debian _) -> preinstallPackage $ mkSystemPackage "openjdk-17-jre"
            _ -> id
        )
      & checkInstalledCommandOutput ("amm" :| ["--version"]) "Ammonite REPL"

  packageSpec $ \_ ->
    ps
      ( (mkGithubPackage "jqlang/jq")
          { Mybox.Package.Github.binaries = ["jq"]
          , Mybox.Package.Github.raw = Left "jq"
          }
      )
      & checkInstalledCommandOutput ("jq" :| ["--version"]) "jq-"

  onlyIf virtualSystem $
    packageSpec $ \_ ->
      ps
        ( (mkGithubPackage "tonsky/FiraCode")
            { Mybox.Package.Github.fonts = ["FiraCode-Regular"]
            }
        )
        & checkInstalledCommandOutput ("fc-list" :| ["FiraCode"]) "FiraCode-Regular"
