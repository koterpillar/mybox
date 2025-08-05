module Mybox.PathSpec where

import Effectful.Exception (evaluate)

import Mybox.Path
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "pRelativeTo" $ do
    let usr = pRoot </> "usr"
    it "returns Nothing for unrelated paths" $
      pRelativeTo usr (pRoot </> "etc" </> "passwd") `shouldBe` Nothing
    it "returns Just relative path for subdirectory paths" $
      pRelativeTo usr (usr </> "local" </> "bin") `shouldBe` Just (pSegment "local" </> "bin")
    it "returns Just empty path for the same path" $ do
      let result = pRelativeTo usr usr
      case result of
        Just p -> p.segments `shouldBe` []
        Nothing -> expectationFailure "Expected Just, got Nothing"
    it "returns Nothing for prefix paths" $
      pRelativeTo (usr </> "local") usr `shouldBe` Nothing
    it "returns Nothing for string-prefix paths" $
      pRelativeTo usr (pRoot </> "usr1") `shouldBe` Nothing

  describe "mkPath" $ do
    describe "for absolute paths" $ do
      it "creates absolute path from absolute string" $
        (mkPath "/usr/local/bin" :: Path Abs) `shouldBe` (pRoot </> "usr" </> "local" </> "bin")
      it "creates absolute path from root" $
        (mkPath "/" :: Path Abs) `shouldBe` pRoot

    describe "for relative paths" $ do
      it "creates relative path from relative string" $
        (mkPath "local/bin" :: Path Rel) `shouldBe` (pSegment "local" </> "bin")
      it "creates relative path from single segment" $
        (mkPath "usr" :: Path Rel) `shouldBe` pSegment "usr"

    describe "for AnyAnchor paths" $ do
      it "creates absolute path from absolute string" $ do
        let result = mkPath "/usr/local" :: Path AnyAnchor
        pAbs result `shouldBe` Just (pRoot </> "usr" </> "local")
        result.segments `shouldBe` ["usr", "local"]
      it "creates relative path from relative string" $ do
        let result = mkPath "usr/local" :: Path AnyAnchor
        pAbs result `shouldBe` Nothing
        result.segments `shouldBe` ["usr", "local"]

  describe "dirname" $ do
    it "returns parent directory for absolute paths" $
      (pRoot </> "usr" </> "local" </> "bin").dirname `shouldBe` (pRoot </> "usr" </> "local")
    it "returns parent directory for relative paths" $
      (pSegment "usr" </> "local" </> "bin").dirname `shouldBe` (pSegment "usr" </> "local")
    it "returns empty segments for single segment relative path" $
      (pSegment "usr").dirname.segments `shouldBe` []
    it "returns root for single segment absolute path" $
      (pRoot </> "usr").dirname `shouldBe` pRoot
    it "throws error for empty paths" $
      evaluate pRoot.dirname `shouldThrow` anyErrorCall

  describe "basename" $ do
    it "returns last segment for absolute paths" $
      (pRoot </> "usr" </> "local" </> "bin").basename `shouldBe` "bin"
    it "returns last segment for relative paths" $
      (pSegment "usr" </> "local" </> "bin").basename `shouldBe` "bin"
    it "returns segment for single segment relative path" $
      (pSegment "usr").basename `shouldBe` "usr"
    it "returns segment for single segment absolute path" $
      (pRoot </> "usr").basename `shouldBe` "usr"
    it "throws error for empty paths" $
      evaluate pRoot.basename `shouldThrow` anyErrorCall
