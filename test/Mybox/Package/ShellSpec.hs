module Mybox.Package.ShellSpec where

import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.Shell
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores
import Mybox.Tracker

spec :: Spec
spec = do
  jsonSpec
    (Nothing @ShellPackage)
    [ (Nothing, "{\"shell\": \"/bin/bash\"}")
    , (Just "with root", "{\"shell\": \"/bin/zsh\", \"root\": true}")
    , (Just "with post commands", "{\"shell\": \"/bin/fish\", \"post\": [\"echo setup complete\"]}")
    ]

  let checkWhoamiShell root = do
        username <- if root then pure "root" else drvUsername
        whoamiResult <- drvRunOutput $ (if root then sudo else id) $ "su" :| [username]
        whoamiResult `shouldBe` username

  onlyIf virtualSystem $ do
    for_ [False, True] $ \root ->
      describe (if root then "root" else "normal user") $ do
        -- cannot test normal user's shell without Docker on GitHub Actions
        skipIf (if root then pure False else inCI) $ do
          packageSpec $ \psa ->
            let sh = "/bin/sh"
                username = if root then "root" else psa.username
             in ps ((mkShellPackage sh){root})
                  & checkInstalledCommandOutput ("grep" :| [username, "/etc/passwd"]) sh
          onlyIf inDocker $ do
            packageSpec $ \_ ->
              ps ((mkShellPackage "/bin/whoami"){root})
                & psName "whoami"
                & checkInstalled (checkWhoamiShell root)

  describe "validation" $
    onlyIf inDocker $
      around (withEff $ runStores . testDriver . nullTrackerSession . runInstallQueue) $ do
        it "fails when shell does not exist" $
          install (mkShellPackage "/bin/xxxxxxxx") `shouldThrow` anyException

        it "fails when shell is not executable" $
          install (mkShellPackage "/etc/shells") `shouldThrow` anyException
