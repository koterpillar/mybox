module Mybox.Package.SkillSpec (spec) where

import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.Skill.Internal
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

skillIgnorePaths :: [Path Rel]
skillIgnorePaths =
  [ ".npm" </> "_cacache"
  , ".npm" </> "_logs"
  , ".npm" </> "_update-notifier-last-checked"
  , ".agents" </> ".skill-lock.json"
  ]

hasSkill :: (Driver :> es, IOE :> es) => Text -> Eff es ()
hasSkill = hasSkill' ".agents"

hasSkill' :: (Driver :> es, IOE :> es) => Text -> Text -> Eff es ()
hasSkill' agentDir name_ = do
  home <- drvHome
  drvIsFile (home </> agentDir </> "skills" </> name_ </> "SKILL.md") >>= (`shouldBe` True)

spec :: Spec
spec = do
  metaSpec
    @SkillPackage
    [ (Just "source only", "{\"skill\": \"test/test\"}")
    , (Just "selected skills", "{\"skill\": \"https://example.com/skills\", \"only\": [\"test\"]}")
    , (Just "agents", "{\"skill\": \"https://example.com/skills\", \"agents\": [\"claude-code\"]}")
    ]
  describe "indexUrl" $ do
    it "appends the well-known path" $
      indexUrl (mkSkillPackage "https://example.com/skill")
        `shouldBe` "https://example.com/skill/.well-known/skills/index.json"
    it "ignores a trailing slash" $
      indexUrl (mkSkillPackage "https://example.com/")
        `shouldBe` "https://example.com/.well-known/skills/index.json"
  describe "remote version" $ do
    withEff (nullTracker . runInstallQueue) $ do
      it "gets version for a well-known source" $ do
        version <- remoteVersion $ mkSkillPackage "https://agentskills.io"
        Text.length version `shouldSatisfy` (>= 10)
      it "fails for a well-known source without an index" $
        remoteVersion (mkSkillPackage "https://example.com") `shouldThrow` anyException
      skipGenericLinux "Default installer is unavailable on generic Linux" $ do
        it "gets version for a repository" $ do
          version <- remoteVersion $ mkSkillPackage "vercel-labs/skills"
          Text.length version `shouldBe` 40
        it "fails for a non-existent repository" $
          remoteVersion (mkSkillPackage "vercel-labs/xxxxxxxxxxxx") `shouldThrow` anyException
  skipGenericLinux "Default installer is unavailable on generic Linux" $ do
    packageSpec $
      ps (mkSkillPackage "DietrichGebert/ponytail")
        & ignorePaths skillIgnorePaths
        & checkInstalled (hasSkill "ponytail")
    packageSpec $
      ps (mkSkillPackage "vercel-labs/skills"){only = Just ["find-skills"]}
        & ignorePaths skillIgnorePaths
        & checkInstalled (hasSkill "find-skills")
    packageSpec $
      ps (mkSkillPackage "https://agentskills.io"){agents = ["claude-code"]}
        & ignorePaths skillIgnorePaths
        & checkInstalled (hasSkill "agent" >> hasSkill' ".claude" "agent")
