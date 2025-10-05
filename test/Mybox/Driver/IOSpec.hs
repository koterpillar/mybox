module Mybox.Driver.IOSpec where

import Data.Set qualified as Set

import Mybox.Driver.Class
import Mybox.Driver.Ops
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  it "returns ExitSuccess for true command" $ do
    result <- drvRunOk $ "true" :| []
    result `shouldBe` ExitSuccess
  it "returns ExitFailure for false command" $ do
    result <- drvRunOk $ "false" :| []
    result `shouldSatisfy` \case
      ExitSuccess -> False
      ExitFailure _ -> True
  it "captures output from echo command" $ do
    result <- drvRunOutput $ "echo" :| ["hello", "world"]
    result `shouldBe` "hello world"
  it "handles empty output correctly" $ do
    result <- drvRunOutput $ "printf" :| [""]
    result `shouldBe` ""
  it "trims whitespace from output" $ do
    result <- drvRunOutput $ "echo" :| ["  trimmed  "]
    result `shouldBe` "trimmed"
  skipIf "Running in Docker modifies commands run" inDocker $ do
    it "reports an error" $
      drvRun ("false" :| []) `shouldThrow` errorCall "Process false failed with exit code 1"
    it "includes error in failure message" $
      shouldThrow
        (drvRun $ shellRaw "echo fail; echo err >&2; false")
        (errorCall "Process /bin/sh '-c' 'echo fail; echo err >&2; false' failed with exit code 1; stderr: err")
  it "writes and reads files" $ do
    let testFile = pRoot </> "tmp" </> "test.txt"
    drvWriteFile testFile "Hello World"
    drvReadFile testFile >>= (`shouldBe` "Hello World")
  describe "drvFind" $
    it "finds files" $
      drvTempDir $ \dir -> do
        let touch p = let p' = dir <//> p in drvMkdir p'.dirname >> drvWriteFile p' ""
        let go = fmap (Set.map $ pRelativeTo_ dir) . drvFind dir
        touch "one"
        touch $ pSegment "subdir" </> "one"
        touch "two"
        touch "three"
        go (mempty{names = Just ["one"]}) >>= (`shouldBe` Set.fromList ["one", "subdir" </> "one"])
        go (mempty{names = Just ["one", "two"]}) >>= (`shouldBe` Set.fromList ["one", "subdir" </> "one", "two"])
        go mempty >>= (`shouldBe` Set.fromList ["one", "subdir", "subdir" </> "one", "two", "three"])
        go (mempty{onlyFiles = True}) >>= (`shouldBe` Set.fromList ["one", "subdir" </> "one", "two", "three"])
        go (mempty{names = Just ["four"]}) >>= (`shouldBe` Set.empty)
  describe "drvFindExecutable" $ do
    it "returns executable path" $ do
      drvFindExecutable ["sh"] >>= (`shouldBe` "sh")
      drvFindExecutable ["nonexistent-command", "sh"] >>= (`shouldBe` "sh")
    it "errors when no executable found" $
      drvFindExecutable ["nonexistent-command"]
        `shouldThrow` errorCall "Neither of nonexistent-command found in PATH."
