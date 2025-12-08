module Mybox.Package.ReleaseSpec where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Filters
import Mybox.Package.Archive
import Mybox.Package.Release.Internal hiding (id)
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
  jsonSpec @ReleasePackage [(Nothing, "{\"repo\": \"example/example\"}")]

  packageSpecGen "neovim" $ \psa ->
    ps
      ( (mkReleasePackage "neovim/neovim")
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

  onlyIfOS "Eza package only provides Linux binaries" (\case Linux _ -> True; _ -> False) $
    packageSpec $
      ps
        ( (mkReleasePackage "eza-community/eza")
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
      ( (mkReleasePackage "com-lihaoyi/Ammonite")
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
      ( (mkReleasePackage "jqlang/jq")
          { archive =
              emptyArchiveFields
                { binaries = ["jq"]
                , raw = Left "jq"
                }
          }
      )
      & checkInstalledCommandOutput ("jq" :| ["--version"]) "jq-"

  packageSpec $
    ps
      ( (mkReleasePackage "koterpillar/hindent-build")
          { archive = emptyArchiveFields{binaries = ["hindent"], raw = Left "hindent"}
          }
      )
      & checkInstalledCommandOutput ("hindent" :| ["--help"]) "Reformat Haskell source code"

  onlyIf "FiraCode font installation tests require virtual system (Docker or CI)" virtualSystem $
    packageSpec $
      ps
        ( (mkReleasePackage "tonsky/FiraCode")
            { archive =
                emptyArchiveFields{fonts = ["FiraCode-Regular"]}
            }
        )
        & checkInstalledCommandOutput ("fc-list" :| ["FiraCode"]) "FiraCode-Regular"

  packageSpec $
    ps
      ( (mkReleasePackage "codeberg.org/mergiraf/mergiraf")
          { archive = emptyArchiveFields{binaries = ["mergiraf"]}
          }
      )
      & checkInstalledCommandOutput ("mergiraf" :| ["--help"]) "A syntax-aware merge driver for Git"

  it "skips release" $ do
    let keytar = (mkReleasePackage "atom/node-keytar"){skipReleases = ["v7.9.0"]}
    r <- release keytar
    r.tag_name `shouldBe` "v7.8.0"

  it "errors when no releases" $ do
    let nixos = mkReleasePackage "NixOS/nixpkgs"
    release nixos `shouldThrow` errorCall "No releases found for NixOS/nixpkgs"

  it "skips prereleases" $ do
    let ha = mkReleasePackage "home-assistant/core"
    let isStable rel = not $ Text.isInfixOf "b" rel.tag_name
    haReleases <- releases ha
    -- Exclude all stable releases _since_ last prerelease
    let latestStable = takeWhile isStable haReleases
    let haSkip = ha{skipReleases = map (.tag_name) latestStable}
    -- Should find the stable release _before_ last prerelease
    r <- release haSkip
    r `shouldSatisfy` isStable

  it "fetches releases when full URL specified" $ do
    let giteaRunner = mkReleasePackage "https://gitea.com/gitea/act_runner"
    release giteaRunner >>= (`shouldSatisfy` \r -> r.prerelease == False)

  it "errors on invalid repo format" $ do
    let invalid = mkReleasePackage "invalid-repo-format"
    release invalid `shouldThrow` errorCall "Invalid repo format: invalid-repo-format"
