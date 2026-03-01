module Mybox.Package.ShellSpec where

import Mybox.Driver
import Mybox.Driver.Test
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.Shell
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

passwd :: Path Abs
passwd = pRoot </> "etc" </> "passwd"

sh :: Path Abs
sh = pRoot </> "bin" </> "sh"

shPackage :: ShellPackage
shPackage = mkShellPackage sh

forRoot :: Applicative m => ((Bool, String) -> m ()) -> m ()
forRoot = for_ [(False, "normal user"), (True, "root")]

spec :: Spec
spec = do
  metaSpec
    @ShellPackage
    [ (Nothing, "{\"shell\": \"/bin/bash\"}")
    , (Just "with root", "{\"shell\": \"/bin/zsh\", \"root\": true}")
    , (Just "with post commands", "{\"shell\": \"/bin/fish\", \"post\": [\"echo setup complete\"]}")
    ]

  let checkWhoamiShell root = do
        username <- if root then pure "root" else drvUsername
        whoamiResult <- drvRunOutput $ (if root then sudo else id) $ "su" :| [username]
        whoamiResult `shouldBe` username

  onlyIf "Shell package tests require virtual system (Docker or CI)" virtualSystem $ do
    forRoot $ \(root, desc) ->
      describe desc $ do
        -- cannot test normal user's shell without Docker on GitHub Actions
        (if root then id else skipIf "Cannot test normal user's shell without Docker on GitHub Actions" inCI) $ do
          packageSpecGen "sh" $ \psa ->
            let username = if root then "root" else psa.username
             in ps (shPackage{root})
                  & checkInstalledCommandOutput ("grep" :| [username, passwd.text]) sh.text
          onlyIf "Whoami shell test requires Docker environment" inDocker $ do
            packageSpec $
              ps ((mkShellPackage $ pRoot </> "bin" </> "whoami"){root})
                & checkInstalled (checkWhoamiShell root)

  withEff (nullTracker . runInstallQueue) $
    describe "local version" $ do
      forRoot $ \(root, desc) ->
        it ("gets shell for " <> desc) $ do
          let package = shPackage{root}
          version <- localVersion package
          version `shouldSatisfy` isJust
          fromJust version `shouldContainText` "sh"
      it "fails on unexpected output" $ do
        let brokenGetShell :: Args -> Maybe Text
            brokenGetShell ("getent" :| _) = Just "unexpected output"
            brokenGetShell ("dscl" :| _) = Just "unexpected output"
            brokenGetShell _ = Nothing
        stubDriver brokenGetShell $
          localVersion shPackage `shouldThrow` anyException

  describe "validation" $
    onlyIf "Shell validation tests require Docker environment" inDocker $
      withEff (nullTracker . runInstallQueue) $ do
        it "fails when shell does not exist" $
          install (mkShellPackage $ pRoot </> "bin" </> "xxxxxxxx") `shouldThrow` anyException

        it "fails when shell is not executable" $
          install (mkShellPackage $ pRoot </> "etc" </> "shells") `shouldThrow` anyException
