module Mybox.Package.GithubSpec where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Filters
import Mybox.Package.Archive
import Mybox.Package.Github.Internal hiding (id)
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
  jsonSpec @GithubPackage [(Nothing, "{\"repo\": \"example/example\"}")]

  packageSpecGen "neovim" $ \psa ->
    ps
      ( (mkGithubPackage "neovim/neovim")
          { archive =
              emptyArchiveFields
                { binaries = ["nvim"]
                , apps = case psa.os of
                    Linux _ -> ["nvim"]
                    _ -> []
                }
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
  onlyIfOS "Eza package only provides Linux binaries" (\case Linux _ -> True; _ -> False) $
    packageSpec $
      ps
        ( (mkGithubPackage "eza-community/eza")
            { archive =
                emptyArchiveFields
                  { binaries = ["eza"]
                  }
            , filters =
                mempty
                  { excludes = [".zip", "no_libgit"]
                  }
            }
        )
        & checkInstalledCommandOutput ("eza" :| ["--version"]) "eza - A modern, maintained replacement for ls"

  packageSpecGen "Ammonite" $ \psa ->
    ps
      ( (mkGithubPackage "com-lihaoyi/Ammonite")
          { archive =
              emptyArchiveFields
                { binaries = ["amm"]
                , raw = Left "amm"
                }
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

  packageSpec $
    ps
      ( (mkGithubPackage "jqlang/jq")
          { archive =
              emptyArchiveFields
                { binaries = ["jq"]
                , raw = Left "jq"
                }
          }
      )
      & checkInstalledCommandOutput ("jq" :| ["--version"]) "jq-"

  onlyIf "hindent-build only provides x86_64 binaries" ((\case X86_64 -> True; _ -> False) <$> drvArchitecture) $
    packageSpec $
      ps
        ( (mkGithubPackage "koterpillar/hindent-build")
            { archive = emptyArchiveFields{binaries = ["hindent"], raw = Left "hindent"}
            }
        )
        & checkInstalledCommandOutput ("hindent" :| ["--help"]) "Reformat Haskell source code"

  onlyIf "FiraCode font installation tests require virtual system (Docker or CI)" virtualSystem $
    packageSpec $
      ps
        ( (mkGithubPackage "tonsky/FiraCode")
            { archive =
                emptyArchiveFields{fonts = ["FiraCode-Regular"]}
            }
        )
        & checkInstalledCommandOutput ("fc-list" :| ["FiraCode"]) "FiraCode-Regular"

  it "skips release" $ do
    let keytar = (mkGithubPackage "atom/node-keytar"){skipReleases = ["v7.9.0"]}
    r <- release keytar
    r.tag_name `shouldBe` "v7.8.0"

  it "errors when no releases" $ do
    let nixos = mkGithubPackage "NixOS/nixpkgs"
    release nixos `shouldThrow` errorCall "No releases found for NixOS/nixpkgs"
