module Mybox.Package.CloneSpec where

import           Data.Function          ((&))

import           Data.List.NonEmpty     (NonEmpty (..))

import           Test.Hspec

import           Mybox.Package.Clone
import           Mybox.Package.SpecBase

spec :: Spec
spec = do
  packageSpec $ \PackageSpecArgs {..} ->
    ps (ClonePackage "ohmyzsh/ohmyzsh" Nothing psaDirectory)
      & psCheckInstalledCommandOutput
          ("cat" :| [psaDirectory <> "/templates/zshrc.zsh-template"])
          "alias ohmyzsh"
  packageSpec $ \PackageSpecArgs {..} ->
    ps (ClonePackage "node-fetch/node-fetch" (Just "2.x") psaDirectory)
      & psCheckInstalledCommandOutput
          ("cat" :| [psaDirectory <> "/package.json"])
          "\"version\": \"2"
