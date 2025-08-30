module Mybox.Package.CloneSpec where

import Mybox.Driver
import Mybox.Package.Clone
import Mybox.Package.SpecBase
import Mybox.Package.System
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    (Nothing @ClonePackage)
    [ (Nothing, "{\"clone\": \"test/test\", \"destination\": \"test\"}")
    , (Just "branch", "{\"clone\": \"test/test\", \"branch\": \"test\", \"destination\": \"test\"}")
    ]
  let baseClone psa =
        ps (mkClonePackage "ohmyzsh/ohmyzsh" $ pWiden psa.directory)
          & checkInstalledCommandOutput
            ("cat" :| [(psa.directory </> "templates" </> "zshrc.zsh-template").text])
            "alias ohmyzsh"
  packageSpecGen "ohmyzsh" baseClone
  packageSpecGen "branch switch" $ \psa ->
    baseClone psa
      & preinstallPackage (mkSystemPackage "git")
      & preinstall (checkoutEarlierCommit psa)
  packageSpecGen "empty directory" $ \psa ->
    baseClone psa
      & preinstall (createEmptyDirectory psa)
  packageSpecGen "node-fetch" $ \psa ->
    ps ((mkClonePackage "node-fetch/node-fetch" $ pWiden psa.directory){branch = Just "2.x"})
      & checkInstalledCommandOutput
        ("cat" :| [(psa.directory </> "package.json").text])
        "\"version\": \"2"

createEmptyDirectory :: Driver :> es => PackageSpecArgs -> Eff es ()
createEmptyDirectory psa = drvMkdir psa.directory

checkoutEarlierCommit :: Driver :> es => PackageSpecArgs -> Eff es ()
checkoutEarlierCommit psa = do
  drvRun $
    "git" :| ["clone", "https://github.com/ohmyzsh/ohmyzsh.git", psa.directory.text]
  drvRun $ "git" :| ["-C", psa.directory.text, "checkout", "HEAD~"]
