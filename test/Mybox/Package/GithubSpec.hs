module Mybox.Package.GithubSpec where

import Mybox.Driver
import Mybox.Package.Github
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec (Nothing @GithubPackage) [(Nothing, "{\"repo\": \"example/example\"}")]
  skipIf ((== Aarch64) <$> drvArchitecture) $
    packageSpec $ \_ ->
      ps ((mkGithubPackage "neovim/neovim"){binaries = ["nvim"]})
        & checkInstalledCommandOutput ("nvim" :| ["--version"]) "NVIM"
