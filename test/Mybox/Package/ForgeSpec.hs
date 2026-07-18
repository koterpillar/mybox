module Mybox.Package.ForgeSpec where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Filters
import Mybox.Package.Archive
import Mybox.Package.Forge.Internal hiding (id)
import Mybox.Package.SpecBase
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Spec.Assertions
import Mybox.SpecBase

-- | Assert that a desktop file exists with the given name and optional executable
assertDesktopFileExists ::
  (Driver :> es, IOE :> es) =>
  Bool -> Text -> Text -> Maybe Text -> Eff es ()
assertDesktopFileExists root fileName expectedName expectedExecutable = do
  local <- drvLocal root
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

libraryExists :: Text -> Driver :> es => Eff es Bool
libraryExists lib = do
  os <- drvOS
  case os of
    MacOS -> pure True
    Linux _ -> do
      libraries <- drvRunOutput $ "ldconfig" :| ["-p"]
      pure $ any (Text.isInfixOf lib) (Text.lines libraries)

neovimPackage :: Bool -> PackageSpecArgs -> PackageSpec ForgePackage
neovimPackage root psa =
  ps
    ( (mkForgePackage "neovim/neovim")
        { archive =
            emptyArchiveFields
              { binaries = ["nvim"]
              , apps = case psa.os of
                  Linux _ -> ["nvim"]
                  _ -> []
              , root = root
              }
        }
    )
    & (if root then preinstallEnableSudo else id)
    & checkInstalled
      ( do
          let nvimCmd = if root then "/usr/local/bin/nvim" else "nvim"
          commandHasOutput (nvimCmd :| ["--version"]) "NVIM"
          case psa.os of
            Linux _ -> do
              assertDesktopFileExists root "nvim" "Neovim" (Just "nvim")
            _ -> pure ()
      )

spec :: Spec
spec = do
  metaSpec @ForgePackage [(Nothing, "{\"repo\": \"example/example\"}")]

  packageSpecGen "neovim" $ neovimPackage False
  packageSpecGen "neovim with root" $ neovimPackage True

  onlyIfOS "Eza package only provides Linux binaries" (\case Linux _ -> True; _ -> False) $
    packageSpec $
      ps
        ( (mkForgePackage "eza-community/eza")
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

  skipGenericLinux "Java installer unavailable on generic Linux" $
    packageSpecGen "Ammonite" $ \psa ->
      ps
        ( (mkForgePackage "com-lihaoyi/Ammonite")
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
      ( (mkForgePackage "jqlang/jq")
          { archive =
              emptyArchiveFields
                { binaries = ["jq"]
                , raw = Left "jq"
                }
          }
      )
      & checkInstalledCommandOutput ("jq" :| ["--version"]) "jq-"

  onlyIf "libgmp required to run hindent" (libraryExists "libgmp") $
    packageSpec $
      ps
        ( (mkForgePackage "koterpillar/hindent-build")
            { archive = emptyArchiveFields{binaries = ["hindent"], raw = Left "hindent"}
            }
        )
        & checkInstalledCommandOutput ("hindent" :| ["--help"]) "Reformat Haskell source code"

  onlyIf "FiraCode font installation tests require virtual system (Docker or CI)" virtualSystem $
    skipGenericLinux "Default installer is unavailable on generic Linux" $
      packageSpec $
        ps
          ( (mkForgePackage "tonsky/FiraCode")
              { archive =
                  emptyArchiveFields{fonts = ["FiraCode-Regular"]}
              }
          )
          & checkInstalledCommandOutput ("fc-list" :| ["FiraCode"]) "FiraCode-Regular"

  packageSpec $
    ps
      ( (mkForgePackage "codeberg.org/mergiraf/mergiraf")
          { archive = emptyArchiveFields{binaries = ["mergiraf"]}
          }
      )
      & checkInstalledCommandOutput ("mergiraf" :| ["--help"]) "A syntax-aware merge driver for Git"

  it "skips release" $ do
    let keytar = (mkForgePackage "atom/node-keytar"){skipReleases = ["v7.9.0"]}
    rs <- matchingReleases keytar
    map (.tag_name) (take 1 rs) `shouldBe` ["v7.8.0"]

  it "returns no releases when all are filtered out" $ do
    let nixos = mkForgePackage "NixOS/nixpkgs"
    matchingReleases nixos >>= (`shouldSatisfy` null)

  it "skips prereleases" $ do
    let ha = mkForgePackage "home-assistant/core"
    let isStable rel = not $ Text.isInfixOf "b" rel.tag_name
    haReleases <- matchingReleases ha
    -- Exclude all stable releases _since_ last prerelease
    let latestStable = takeWhile isStable haReleases
    let haSkip = ha{skipReleases = map (.tag_name) latestStable}
    -- Should find the stable release _before_ last prerelease
    filtered <- matchingReleases haSkip
    map (.tag_name) (take 1 filtered) `shouldSatisfy` all (not . Text.isInfixOf "b")

  it "fetches releases when full URL specified" $ do
    let giteaRunner = mkForgePackage "https://gitea.com/gitea/act_runner"
    matchingReleases giteaRunner >>= (`shouldSatisfy` \rs -> all (not . (.prerelease)) (take 1 rs))

  it "errors on invalid repo format" $ do
    let invalid = mkForgePackage "invalid-repo-format"
    matchingReleases invalid `shouldThrow` errorCall "Invalid repo format: invalid-repo-format"
