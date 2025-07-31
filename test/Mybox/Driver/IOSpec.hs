module Mybox.Driver.IOSpec where

import Data.Set qualified as Set
import Data.Text qualified as Text

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
  it "writes and reads files" $ do
    drvWriteFile "test.txt" "Hello World"
    drvReadFile "test.txt" >>= (`shouldBe` "Hello World")
  describe "drvFind" $
    it "finds files" $
      drvTempDir $ \dir -> do
        let touch p = let p' = dir </> p in drvMkdir (pDirname p') >> drvWriteFile p' ""
        let strip p = fromMaybe ("NOT IN DIR: " <> p) $ Text.stripPrefix (dir </> "") p
        let go = fmap (Set.map strip) . drvFind dir
        touch "one"
        touch "subdir/one"
        touch "two"
        touch "three"
        go (mempty{names = Just ["one"]}) >>= (`shouldBe` Set.fromList ["one", "subdir" </> "one"])
        go (mempty{names = Just ["one", "two"]}) >>= (`shouldBe` Set.fromList ["one", "subdir" </> "one", "two"])
        go mempty >>= (`shouldBe` Set.fromList ["one", "subdir", "subdir" </> "one", "two", "three"])
        go (mempty{onlyFiles = True}) >>= (`shouldBe` Set.fromList ["one", "subdir" </> "one", "two", "three"])
        go (mempty{names = Just ["four"]}) >>= (`shouldBe` Set.empty)
