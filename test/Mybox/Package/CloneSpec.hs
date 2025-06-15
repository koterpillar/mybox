module Mybox.Package.CloneSpec where

import           Test.Hspec

import           Mybox.Driver
import           Mybox.Package.Clone
import           Mybox.Package.SpecBase
import           Mybox.Prelude

spec :: Spec
spec = do
  let baseClone (PackageSpecArgs {..}) =
        ps (ClonePackage "ohmyzsh/ohmyzsh" Nothing psaDirectory)
          & psCheckInstalledCommandOutput
              ("cat" :| [psaDirectory <> "/templates/zshrc.zsh-template"])
              "alias ohmyzsh"
  packageSpec baseClone
  let checkoutEarlierCommit :: MonadDriver m => Text -> m ()
      checkoutEarlierCommit dir = do
        drvRun
          $ "git" :| ["clone", "https://github.com/ohmyzsh/ohmyzsh.git", dir]
        drvRun $ "git" :| ["-C", dir, "checkout", "HEAD~"]
  packageSpec $ \psa ->
    baseClone psa
      & psName "branch switch"
      & psPreinstall (checkoutEarlierCommit (psaDirectory psa))
  packageSpec $ \PackageSpecArgs {..} ->
    ps (ClonePackage "node-fetch/node-fetch" (Just "2.x") psaDirectory)
      & psCheckInstalledCommandOutput
          ("cat" :| [psaDirectory <> "/package.json"])
          "\"version\": \"2"
