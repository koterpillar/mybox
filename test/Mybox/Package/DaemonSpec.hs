module Mybox.Package.DaemonSpec where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Package.Daemon
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    (Nothing @DaemonPackage)
    [ (Nothing, "{\"daemon\": [\"echo\", \"hello\"]}")
    , (Just "single command", "{\"daemon\": \"true\"}")
    , (Just "with name override", "{\"daemon\": [\"sleep\", \"3600\"], \"name\": \"my-sleep\"}")
    ]

  describe "name generation" $ do
    it "uses command for default name" $ do
      let pkg = mkDaemonPackage ("echo" :| ["hello", "world"])
      pkg.name `shouldBe` "echo hello world"

    it "uses name override when provided" $ do
      let pkg = (mkDaemonPackage ("sleep" :| ["60"])){nameOverride = Just "my-sleep"}
      pkg.name `shouldBe` "my-sleep"

  describe "daemon names" $ do
    let pkg = mkDaemonPackage $ "echo" :| ["multiple words", "привет мир", "/etc/passwd"]
    it "produces clean name" $ do
      daemonName pkg `shouldBe` "com.koterpillar.mybox.echo_multiple_words_привет_мир__etc_passwd"
    it "produces description" $ do
      daemonDescription pkg `shouldBe` "Mybox: echo 'multiple words' 'привет мир' /etc/passwd"

  onlyIf "Daemon tests require CI environment" inCI $
    skipIf "Daemon tests cannot run in Docker" inDocker $
      packageSpecGen "test daemon" $ \psa -> do
        ps (mkDaemonPackage ("sleep" :| ["3600"]))
          & checkInstalledCommandOutput
            ( case psa.os of
                Linux _ -> "systemctl" :| ["--user", "list-units"]
                MacOS -> "launchctl" :| ["list"]
            )
            "Mybox: sleep 3600"
          & cleanup
            ( case psa.os of
                Linux _ -> cleanupLinux
                MacOS -> cleanupMacOS
            )

cleanupLinux :: Driver :> es => Eff es ()
cleanupLinux = do
  local <- drvLocal
  let serviceFile = local </> "share" </> "systemd" </> "user" </> "com.koterpillar.mybox.sleep_3600.service"
  drvRm serviceFile
  drvRun $ "systemctl" :| ["--user", "daemon-reload"]

cleanupMacOS :: Driver :> es => Eff es ()
cleanupMacOS = do
  daemons <- drvRunOutput $ "launchctl" :| ["list"]
  when ("Mybox" `Text.isInfixOf` daemons) $ do
    drvRun $ "launchctl" :| ["unload", "com.koterpillar.mybox.sleep_3600"]

  home <- drvHome
  let plistFile = home </> "Library" </> "LaunchAgents" </> "com.koterpillar.mybox.sleep_3600.plist"
  drvRm plistFile
