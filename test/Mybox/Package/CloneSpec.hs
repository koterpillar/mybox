module Mybox.Package.CloneSpec where

import Test.Hspec

import Mybox.Driver
import Mybox.Package.Clone
import Mybox.Package.SpecBase
import Mybox.Prelude

spec :: Spec
spec = do
  jsonSpec
    (Nothing @ClonePackage)
    [ (Nothing, "{\"clone\": \"test/test\", \"destination\": \"test\"}")
    , (Just "branch", "{\"clone\": \"test/test\", \"branch\": \"test\", \"destination\": \"test\"}")
    ]
  let baseClone psa =
        ps (ClonePackage "ohmyzsh/ohmyzsh" Nothing psa.directory)
          & checkInstalledCommandOutput
            ("cat" :| [psa.directory </> "templates" </> "zshrc.zsh-template"])
            "alias ohmyzsh"
  packageSpec baseClone
  packageSpec $ \psa ->
    baseClone psa
      & psName "branch switch"
      & preinstall (checkoutEarlierCommit psa)
  packageSpec $ \psa ->
    baseClone psa
      & psName "empty directory"
      & preinstall (createEmptyDirectory psa)
  packageSpec $ \psa ->
    ps (ClonePackage "node-fetch/node-fetch" (Just "2.x") psa.directory)
      & checkInstalledCommandOutput
        ("cat" :| [psa.directory </> "package.json"])
        "\"version\": \"2"

createEmptyDirectory :: Driver :> es => PackageSpecArgs -> Eff es ()
createEmptyDirectory psa = drvMkdir psa.directory

checkoutEarlierCommit :: Driver :> es => PackageSpecArgs -> Eff es ()
checkoutEarlierCommit psa = do
  drvRun $
    "git" :| ["clone", "https://github.com/ohmyzsh/ohmyzsh.git", psa.directory]
  drvRun $ "git" :| ["-C", psa.directory, "checkout", "HEAD~"]
