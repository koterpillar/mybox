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
              ("cat" :| [psaDirectory </> "templates" </> "zshrc.zsh-template"])
              "alias ohmyzsh"
  packageSpec baseClone
  packageSpec $ \psa ->
    baseClone psa
      & psName "branch switch"
      & psPreinstall (checkoutEarlierCommit psa)
  packageSpec $ \psa ->
    baseClone psa
      & psName "empty directory"
      & psPreinstall (createEmptyDirectory psa)
  packageSpec $ \PackageSpecArgs {..} ->
    ps (ClonePackage "node-fetch/node-fetch" (Just "2.x") psaDirectory)
      & psCheckInstalledCommandOutput
          ("cat" :| [psaDirectory </> "package.json"])
          "\"version\": \"2"

createEmptyDirectory :: Driver :> es => PackageSpecArgs -> Eff es ()
createEmptyDirectory PackageSpecArgs {..} = drvMkdir psaDirectory

checkoutEarlierCommit :: Driver :> es => PackageSpecArgs -> Eff es ()
checkoutEarlierCommit PackageSpecArgs {..} = do
  drvRun
    $ "git" :| ["clone", "https://github.com/ohmyzsh/ohmyzsh.git", psaDirectory]
  drvRun $ "git" :| ["-C", psaDirectory, "checkout", "HEAD~"]
