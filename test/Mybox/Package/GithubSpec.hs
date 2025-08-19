module Mybox.Package.GithubSpec where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Filters
import Mybox.Package.Archive
import Mybox.Package.Github
import Mybox.Package.SpecBase
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Spec.Assertions
import Mybox.SpecBase

-- | Assert that a desktop file exists with the given name and optional executable
assertDesktopFileExists ::
  (Driver :> es, IOE :> es) =>
  Text -> Text -> Maybe Text -> Eff es ()
assertDesktopFileExists fileName expectedName expectedExecutable = do
  local <- drvLocal
  let desktopFilePath = local </> "share" </> "applications" </> (fileName <> ".desktop")
  desktopContent <- parseDesktopFile <$> drvReadFile desktopFilePath

  Map.lookup "Name" desktopContent `shouldBe` Just expectedName

  for_ expectedExecutable $ \exec -> do
    command <- assertKeyExists "desktop file" "Exec" desktopContent
    command `shouldContainText` exec

  let iconsDir = local </> "share" </> "icons"
  for_ (Map.lookup "Icon" desktopContent) $ \iconName ->
    assertAnyFileExists ("Icon " <> iconName) iconsDir (iconPaths iconName)

pngResolutions :: [Text]
pngResolutions = do
  p :: Int <- [4 .. 8]
  let res :: Int = 2 ^ p
  let resStr = Text.pack $ show res
  pure $ resStr <> "x" <> resStr

iconPaths :: Text -> [Path Rel]
iconPaths name = do
  (base, ext) <- case Text.splitOn "." name of
    [base_, ext_] -> pure (base_, ext_)
    [base_] -> (base_,) <$> ["png", "svg"]
    _ -> terror $ "Unexpected icon name: " <> name
  size <- case ext of
    "png" -> pngResolutions
    "svg" -> ["scalable"]
    _ -> terror $ "Unexpected icon extension: " <> ext
  pure $ "hicolor" </> size </> "apps" </> (base <> "." <> ext)

spec :: Spec
spec = do
  jsonSpec (Nothing @GithubPackage) [(Nothing, "{\"repo\": \"example/example\"}")]

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
            { binaries = ["eza"]
            , filters =
                mempty
                  { excludes = [".zip", "no_libgit"]
                  }
            }
        )
        & checkInstalledCommandOutput ("eza" :| ["--version"]) "eza - A modern, maintained replacement for ls"

  packageSpec $ \psa ->
    ps
      ( (mkGithubPackage "com-lihaoyi/Ammonite")
          { binaries = ["amm"]
          , raw = Left "amm"
          , filters =
              mempty
                { prefixes = ["3.6-"]
                , suffixes = ["-bootstrap"]
                }
          }
      )
      & ( case psa.os of
            Linux Fedora -> preinstallPackage $ mkSystemPackage "java-latest-openjdk"
            Linux (Debian _) -> preinstallPackage $ mkSystemPackage "default-jre"
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
