module Mybox.Package.AgentSettingsSpec (spec) where

import Control.Lens qualified as Lens
import Data.Aeson.Lens qualified as Lens
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Driver.Test
import Mybox.Package.AgentSettings.Internal
import Mybox.Package.AgentSettings.Types
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.Some
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  metaSpec @AgentSettingsPackage
    [ (Nothing, "{\"agent-settings\":\"tools\",\"agents\":[\"claude\",\"rovo\"]}")
    , (Just "rules", "{\"agent-settings\":\"tools\",\"agents\":\"claude\",\"read\":[\"projects\",\"/etc/hosts\"],\"write\":\"projects\",\"commands\":[\"git status\",{\"exact\":\"make test\"}]}")
    ]
  describe "decoding" $ do
    for_
      [ "{\"agents\":[\"claude\"]}"
      , "{\"agent-settings\":\"tools\"}"
      , "{\"agent-settings\":\"tools\",\"agents\":[]}"
      , "{\"agent-settings\":\"tools\",\"agents\":[\"unknown\"]}"
      , "{\"agent-settings\":\"tools\",\"agents\":[\"kilo\"]}"
      , "{\"agent-settings\":\"tools\",\"agents\":[\"codex\"]}"
      , "{\"agent-settings\":\"tools\",\"agents\":[\"claude\"],\"typo\":[]}"
      ]
      $ \input ->
        it ("rejects " <> Text.unpack input) $
          eitherDecodeStrictText @AgentSettingsPackage input `shouldSatisfy` isLeft
    for_ ["\"\"", "\"git  status\"", "\"git status && echo hi\"", "\"git *\"", "{\"exact\":\"\"}", "{\"exact\":\"git status\",\"typo\":true}"] $ \input ->
      it ("rejects command " <> Text.unpack input) $
        eitherDecodeStrictText @CommandRule input `shouldSatisfy` isLeft
    it "registers the package in SomePackage" $ do
      let p = (mkAgentSettingsPackage "tools" $ Claude :| []){commands = [CommandExact "make test"]}
      eitherDecodeStrictText @SomePackage (jsonEncode p) `shouldBe` Right (mkSomePackage p)
  describe "Claude" $ do
    it "merges grants, preserving unrelated settings and denials" $ do
      original <- jsonDecode @Value "settings" "{\"model\":\"custom\",\"permissions\":{\"deny\":[\"Bash(rm *)\"],\"allow\":[\"Bash(pwd)\"]}}"
      let update = updateSettings ClaudeConfig [mkPath "/home/test/code"] [mkPath "/tmp"] [CommandPrefix "git status", CommandExact "make test"]
      updated <- throwLeft $ update original
      update updated `shouldBe` Right updated
      let output = jsonEncode updated
      output `shouldContainText` "Read(//home/test/code/**)"
      output `shouldContainText` "Edit(//tmp/**)"
      output `shouldContainText` "Bash(git status *)"
      output `shouldContainText` "Bash(make test)"
      output `shouldContainText` "\"deny\":[\"Bash(rm *)\"]"
      output `shouldContainText` "\"model\":\"custom\""
    for_ ["null", "[]", "{\"permissions\":true}", "{\"permissions\":{\"allow\":{}}}"] $ \input ->
      it ("skips wrong-type existing settings " <> Text.unpack input) $ do
        existing <- jsonDecode @Value "settings" input
        updateSettings ClaudeConfig [] [] [CommandPrefix "git status"] existing `shouldBe` Right existing
  describe "Rovo" $ do
    it "generates anchored, escaped regexes for exact and prefix rules" $ do
      let update = updateSettings RovoConfig [mkPath "/tmp"] [mkPath "/tmp"] [CommandPrefix "./bin/lint", CommandExact "make test"]
      updated <- throwLeft $ update (object [])
      jsonEncode updated `shouldContainText` "^make test$"
      jsonEncode updated `shouldContainText` "^\\\\./bin/lint( .*)?$"
      update updated `shouldBe` Right updated
    it "rejects read-only external paths" $
      updateSettings RovoConfig [mkPath "/private"] [] [] (object []) `shouldSatisfy` isLeft
    it "prepends allow rules before existing command restrictions" $ do
      original <- jsonDecode @Value "settings" "{\"toolPermissions\":{\"bash\":{\"commands\":[{\"command\":\"git.*\",\"permission\":\"deny\"}]}}}"
      updated <- throwLeft $ updateSettings RovoConfig [] [] [CommandPrefix "git"] original
      let allow = object ["command" .= ("^git( .*)?$" :: Text), "permission" .= ("allow" :: Text)]
          deny = object ["command" .= ("git.*" :: Text), "permission" .= ("deny" :: Text)]
      (updated Lens.^? rovoCommands) `shouldBe` Just [allow, deny]
      updateSettings RovoConfig [] [] [CommandPrefix "git"] updated `shouldBe` Right updated
    for_ ["{\"toolPermissions\":null}", "{\"toolPermissions\":{\"bash\":[]}}", "{\"toolPermissions\":{\"bash\":{\"commands\":null}}}"] $ \input ->
      it ("skips wrong-type existing settings " <> Text.unpack input) $ do
        existing <- jsonDecode @Value "settings" input
        updateSettings RovoConfig [] [] [CommandPrefix "git status"] existing `shouldBe` Right existing
  describe "missing keys" $ do
    it "creates Claude parents and an explicit empty array" $ do
      expected <- jsonDecode @Value "settings" "{\"permissions\":{\"allow\":[]}}"
      Lens.over claudeSettings id (object []) `shouldBe` expected
    it "creates Rovo command parents and an explicit empty array" $ do
      expected <- jsonDecode @Value "settings" "{\"toolPermissions\":{\"bash\":{\"commands\":[]}}}"
      Lens.over rovoCommands id (object []) `shouldBe` expected
    it "creates Rovo path parents and an explicit empty array" $ do
      expected <- jsonDecode @Value "settings" "{\"toolPermissions\":{\"allowedExternalPaths\":[]}}"
      Lens.over rovoPaths id (object []) `shouldBe` expected
  withEff (nullTracker . runInstallQueue) $ describe "installation" $ do
    it "updates only the isolated settings, preserves symlinks and file modes" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let target = home </> "claude-original.json"
            settings = home <//> mkPath @Rel ".claude/settings.json"
            original = "{\"model\":\"custom\"}"
            p = (mkAgentSettingsPackage "tools" $ Claude :| [Rovo]){commands = [CommandPrefix "git status"], write = [mkPath "code"]}
        drvWriteFile target original
        drvRun $ "chmod" :| ["600", target.text]
        drvLink target settings
        install p
        drvIsSymlink settings >>= (`shouldBe` True)
        mode <- Text.take 10 <$> drvRunOutput ("ls" :| ["-ld", target.text])
        mode `shouldBe` "-rw-------"
        drvIsFile (mkPath @Abs $ target.text <> ".mybox-backup") >>= (`shouldBe` False)
        drvIsFile (home <//> mkPath @Rel ".rovo/config.yml") >>= (`shouldBe` True)
        drvIsFile (home <//> mkPath @Rel ".rovodev/config.yml") >>= (`shouldBe` False)
        localVersion p >>= (`shouldBe` Just "configured")
        installed <- drvReadFile settings
        install p
        drvReadFile settings >>= (`shouldBe` installed)
    it "keeps named packages for the same agents installed independently" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let git = (mkAgentSettingsPackage "git" $ Claude :| [Rovo]){commands = [CommandPrefix "git status"]}
            stack = (mkAgentSettingsPackage "stack" $ Claude :| [Rovo]){commands = [CommandPrefix "stack test"]}
        getName git `shouldBe` "agent-settings#git"
        getName stack `shouldBe` "agent-settings#stack"
        withoutName git `shouldBe` Just git{name = ""}
        ensureInstalled git
        ensureInstalled stack
        isInstalled git >>= (`shouldBe` True)
        isInstalled stack >>= (`shouldBe` True)
        ensureInstalled git
        isInstalled stack >>= (`shouldBe` True)
    it "treats wrong-type skipped fields as configured without rewriting them" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let settings = home <//> mkPath @Rel ".claude/settings.json"
            original = "{\"permissions\":{\"allow\":false}}"
            p = (mkAgentSettingsPackage "tools" $ Claude :| []){commands = [CommandPrefix "git status"]}
        drvWriteFile settings original
        ensureInstalled p
        isInstalled p >>= (`shouldBe` True)
        drvReadFile settings >>= (`shouldBe` original)
        drvIsFile (mkPath @Abs $ settings.text <> ".mybox-backup") >>= (`shouldBe` False)
    it "preserves trailing newlines in unrelated YAML block strings" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let settings = home <//> settingsFile RovoConfig
            original = "note: |+\n  keep these newlines\n\n"
            p = (mkAgentSettingsPackage "tools" $ Rovo :| []){commands = [CommandPrefix "git status"]}
        drvMkdir settings.dirname
        drvRun $ shellRaw $ "printf '%s' " <> shellQuote original <> " > " <> shellQuote settings.text
        install p
        updated <- drvReadBinaryFile settings
        let value = updated Lens.^? settingsFormat RovoConfig
        (value >>= (Lens.^? Lens.key "note")) `shouldBe` Just (String "keep these newlines\n\n")
    for_ [(Claude, ".claude/settings.json"), (Rovo, ".rovo/config.yml")] $ \(agent, file) ->
      it ("rejects invalid settings without overwriting them for " <> show agent) $
        drvTempDir $ \home -> stubDriver (homeDriver home) $ do
          let settings = home <//> mkPath @Rel file
              p = (mkAgentSettingsPackage "tools" $ agent :| []){commands = [CommandPrefix "git status"]}
          drvWriteFile settings "{"
          original <- drvReadBinaryFile settings
          install p `shouldThrow` anyException
          drvReadBinaryFile settings >>= (`shouldBe` original)
    it "writes each agent directly before moving to the next" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let p = (mkAgentSettingsPackage "tools" $ Claude :| [Rovo]){read = [mkPath "code"]}
        install p `shouldThrow` anyException
        drvIsFile (home <//> mkPath @Rel ".claude/settings.json") >>= (`shouldBe` True)
    it "reinstalls only when the package hash changes" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let settings = home <//> mkPath @Rel ".claude/settings.json"
            p = (mkAgentSettingsPackage "tools" $ Claude :| []){commands = [CommandPrefix "git status"]}
            changed = p{commands = [CommandPrefix "stack test"]}
        ensureInstalled p
        drvWriteFile settings "{}"
        ensureInstalled p
        drvReadFile settings >>= (`shouldBe` "{}")
        isInstalled changed >>= (`shouldBe` False)
        ensureInstalled changed
        drvReadFile settings >>= (`shouldContainText` "Bash(stack test *)")

homeDriver :: Path Abs -> Args -> Maybe Text
homeDriver home args
  | args == shell ("eval" :| ["echo", "~"]) = Just home.text
  | otherwise = Nothing
