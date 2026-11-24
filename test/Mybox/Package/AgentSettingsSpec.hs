module Mybox.Package.AgentSettingsSpec (spec) where

import Control.Lens qualified as Lens
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Data.Yaml qualified as Yaml

import Mybox.Aeson
import Mybox.Driver
import Mybox.Driver.Test
import Mybox.FileLens qualified as Lens
import Mybox.Package.AgentSettings.Internal
import Mybox.Package.AgentSettings.Types
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  metaSpec @AgentSettingsPackage
    [ (Just "rules", "{\"agent-settings\":\"tools\",\"agents\":\"claude\",\"read\":[\"projects\",\"/etc/hosts\"],\"write\":\"projects\",\"execute\":[\"git status\",{\"exact\":\"make test\"}]}")
    ]
  describe "decoding" $ do
    for_ ["\"\"", "\"git  status\"", "\"git status && echo hi\"", "\"git *\"", "{\"exact\":\"\"}", "{\"exact\":\"git status\",\"typo\":true}"] $ \input ->
      it ("rejects command " <> Text.unpack input) $
        eitherDecodeStrictText @CommandRule input `shouldSatisfy` isLeft
  describe "Claude" $ do
    it "merges grants, preserving unrelated settings and denials" $ do
      original <- jsonDecode @Value "settings" "{\"model\":\"custom\",\"permissions\":{\"deny\":[\"Bash(rm *)\"],\"allow\":[\"Bash(pwd)\"]}}"
      let update = updateSettings ClaudeConfig (AgentSettings [mkPath "/home/test/code"] [mkPath "/tmp"] [testCommand Prefix "git status", testCommand Exact "make test"])
      let updated = update original
      let output = jsonEncode updated
      output `shouldContainText` "Read(//home/test/code/**)"
      output `shouldContainText` "Read(//tmp/**)"
      output `shouldContainText` "Edit(//tmp/**)"
      output `shouldContainText` "Bash(git status *)"
      output `shouldContainText` "Bash(make test)"
      output `shouldContainText` "\"deny\":[\"Bash(rm *)\"]"
      output `shouldContainText` "\"model\":\"custom\""
    for_ ["null", "[]", "{\"permissions\":true}", "{\"permissions\":{\"allow\":{}}}"] $ \input ->
      it ("skips wrong-type existing settings " <> Text.unpack input) $ do
        existing <- jsonDecode @Value "settings" input
        updateSettings ClaudeConfig (AgentSettings [] [] [testCommand Prefix "git status"]) existing `shouldBe` existing
  describe "Rovo" $ do
    it "generates anchored, escaped regexes for exact and prefix rules" $ do
      let update = updateSettings RovoConfig (AgentSettings [mkPath "/tmp"] [mkPath "/tmp"] [testCommand Prefix "./bin/lint", testCommand Exact "make test"])
      let updated = update (object [])
      jsonEncode updated `shouldContainText` "^make test$"
      jsonEncode updated `shouldContainText` "^\\\\./bin/lint( .*)?$"
    it "rejects read-only external paths" $
      validatePaths RovoConfig (AgentSettings [mkPath "/private"] [] []) `shouldSatisfy` isLeft
    it "uses one external path grant when a write path covers a read path" $ do
      let settings = AgentSettings [mkPath "/tmp/code"] [mkPath "/tmp"] []
      validatePaths RovoConfig settings `shouldBe` Right ()
      (updateSettings RovoConfig settings (object []) Lens.^? rovoPaths)
        `shouldBe` Just (Vector.singleton (String "/tmp"))
    it "prepends allow rules before existing command restrictions" $ do
      original <- jsonDecode @Value "settings" "{\"toolPermissions\":{\"bash\":{\"commands\":[{\"command\":\"git.*\",\"permission\":\"deny\"}]}}}"
      let update = updateSettings RovoConfig (AgentSettings [] [mkPath "/tmp"] [testCommand Prefix "git"])
          updated = update original
      let allow = object ["command" .= ("^git( .*)?$" :: Text), "permission" .= ("allow" :: Text)]
          deny = object ["command" .= ("git.*" :: Text), "permission" .= ("deny" :: Text)]
      (updated Lens.^? rovoCommands) `shouldBe` Just (Vector.fromList [allow, deny])
      let repeated = update updated
      (repeated Lens.^? rovoCommands) `shouldBe` Just (Vector.fromList [allow, allow, deny])
      (repeated Lens.^? rovoPaths) `shouldBe` Just (Vector.fromList [String "/tmp", String "/tmp"])
    for_ ["{\"toolPermissions\":null}", "{\"toolPermissions\":{\"bash\":[]}}", "{\"toolPermissions\":{\"bash\":{\"commands\":null}}}"] $ \input ->
      it ("skips wrong-type existing settings " <> Text.unpack input) $ do
        existing <- jsonDecode @Value "settings" input
        updateSettings RovoConfig (AgentSettings [] [] [testCommand Prefix "git status"]) existing `shouldBe` existing
  describe "missing keys" $ do
    it "creates Claude parents and an allow array when adding grants" $ do
      expected <- jsonDecode @Value "settings" "{\"permissions\":{\"allow\":[\"Bash(pwd)\"]}}"
      Lens.set claudeSettings (Vector.singleton $ String "Bash(pwd)") (object []) `shouldBe` expected
    it "creates Rovo command parents and an array when adding grants" $ do
      expected <- jsonDecode @Value "settings" "{\"toolPermissions\":{\"bash\":{\"commands\":[{\"command\":\"^pwd$\",\"permission\":\"allow\"}]}}}"
      Lens.set rovoCommands (Vector.singleton $ commandRule RovoConfig $ testCommand Exact "pwd") (object []) `shouldBe` expected
    it "creates Rovo path parents and an array when adding grants" $ do
      expected <- jsonDecode @Value "settings" "{\"toolPermissions\":{\"allowedExternalPaths\":[\"/tmp\"]}}"
      Lens.set rovoPaths (Vector.singleton $ String "/tmp") (object []) `shouldBe` expected
    it "keeps empty defaults absent" $ do
      Lens.over claudeSettings id (object []) `shouldBe` object []
      Lens.over rovoCommands id (object []) `shouldBe` object []
      Lens.over rovoPaths id (object []) `shouldBe` object []
  withEff (nullTracker . runInstallQueue) $ describe "installation" $ do
    it "creates and updates settings through the shared file modifier" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let settings = home <//> mkPath @Rel ".claude/settings.json"
            original = "{\"model\":\"custom\"}"
            p = (mkAgentSettingsPackage "tools" $ Claude :| [Rovo]){settings = AgentSettings [] [mkPath "code"] [testCommand Prefix "git status"]}
        drvWriteFile settings original
        ensureInstalled p
        drvIsFile (home <//> mkPath @Rel ".rovo/config.yml") >>= (`shouldBe` True)
        drvIsFile (home <//> mkPath @Rel ".rovodev/config.yml") >>= (`shouldBe` False)
        localVersion p >>= (`shouldBe` Just "configured")
        installed <- drvReadFile settings
        ensureInstalled p
        drvReadFile settings >>= (`shouldBe` installed)
    it "keeps named packages for the same agents installed independently" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let git = (mkAgentSettingsPackage "git" $ Claude :| [Rovo]){settings = AgentSettings [] [] [testCommand Prefix "git status"]}
            stack = (mkAgentSettingsPackage "stack" $ Claude :| [Rovo]){settings = AgentSettings [] [] [testCommand Prefix "stack test"]}
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
            p = (mkAgentSettingsPackage "tools" $ Claude :| []){settings = AgentSettings [] [] [testCommand Prefix "git status"]}
        drvWriteFile settings original
        ensureInstalled p
        isInstalled p >>= (`shouldBe` True)
        drvReadFile settings >>= (`shouldBe` original)
        drvIsFile (mkPath @Abs $ settings.text <> ".mybox-backup") >>= (`shouldBe` False)
    it "preserves trailing newlines in unrelated YAML block strings" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let settings = home <//> settingsFile RovoConfig
            original = "note: |+\n  keep these newlines\n\n"
            p = (mkAgentSettingsPackage "tools" $ Rovo :| []){settings = AgentSettings [] [] [testCommand Prefix "git status"]}
        drvMkdir settings.dirname
        drvRun $ shellRaw $ "printf '%s' " <> shellQuote original <> " > " <> shellQuote settings.text
        install p
        updated <- drvReadBinaryFile settings
        value <- Yaml.decodeThrow updated
        ((value :: Value) Lens.^? Lens.key "note") `shouldBe` Just (String "keep these newlines\n\n")
    for_ [(Claude, ".claude/settings.json"), (Rovo, ".rovo/config.yml")] $ \(agent, file) ->
      it ("preserves invalid settings without overwriting them for " <> show agent) $
        drvTempDir $ \home -> stubDriver (homeDriver home) $ do
          let settings = home <//> mkPath @Rel file
              p = (mkAgentSettingsPackage "tools" $ agent :| []){settings = AgentSettings [] [] [testCommand Prefix "git status"]}
          drvWriteFile settings "{"
          original <- drvReadBinaryFile settings
          install p
          drvReadBinaryFile settings >>= (`shouldBe` original)
    it "preserves YAML comments and formatting when no grants are requested" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let settings = home <//> settingsFile RovoConfig
            original = "# keep this comment\ntoolPermissions:\n  bash:\n    commands: [{command: '^pwd$', permission: allow}]\n"
            p = mkAgentSettingsPackage "tools" (Rovo :| [])
        drvWriteFile settings original
        originalBytes <- drvReadBinaryFile settings
        install p
        drvReadBinaryFile settings >>= (`shouldBe` originalBytes)
    it "writes each agent directly before moving to the next" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let p = (mkAgentSettingsPackage "tools" $ Claude :| [Rovo]){settings = AgentSettings [mkPath "code"] [] []}
        install p `shouldThrow` anyException
        drvIsFile (home <//> mkPath @Rel ".claude/settings.json") >>= (`shouldBe` True)
    it "reinstalls only when the package hash changes" $
      drvTempDir $ \home -> stubDriver (homeDriver home) $ do
        let settings = home <//> mkPath @Rel ".claude/settings.json"
            p = (mkAgentSettingsPackage "tools" $ Claude :| []){settings = AgentSettings [] [] [testCommand Prefix "git status"]}
            changed = p{settings = p.settings{execute = [testCommand Prefix "stack test"]}}
        ensureInstalled p
        drvWriteFile settings "{}"
        ensureInstalled p
        drvReadFile settings >>= (`shouldBe` "{}")
        isInstalled changed >>= (`shouldBe` False)
        ensureInstalled changed
        drvReadFile settings >>= (`shouldContainText` "Bash(stack test *)")

testCommand :: HasCallStack => CommandMatch -> Text -> CommandRule
testCommand match = either error id . mkCommandRule match

homeDriver :: Path Abs -> Args -> Maybe Text
homeDriver home args
  | args == shell ("eval" :| ["echo", "~"]) = Just home.text
  | otherwise = Nothing
