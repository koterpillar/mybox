module Mybox.PathSpec where

import Mybox.Path
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "pRelativeTo_" $ do
    let usr = pRoot </> "usr"
    it "errors for unrelated paths" $
      evaluate (pRelativeTo_ usr $ pRoot </> "etc" </> "passwd")
        `shouldThrow` errorCall "Cannot get relative path from mkPath \"/usr\" to mkPath \"/etc/passwd\""
    it "returns Just relative path for subdirectory paths" $
      pRelativeTo_ usr (usr </> "local" </> "bin") `shouldBe` (pSegment "local" </> "bin")
    it "returns Just empty path for the same path" $ do
      let result = pRelativeTo_ usr usr
      result.segments `shouldBe` []
    it "errors for prefix paths" $
      evaluate (pRelativeTo_ (usr </> "local") usr)
        `shouldThrow` errorCall "Cannot get relative path from mkPath \"/usr/local\" to mkPath \"/usr\""
    it "errors for string-prefix paths" $
      evaluate (pRelativeTo_ usr $ pRoot </> "usr1")
        `shouldThrow` errorCall "Cannot get relative path from mkPath \"/usr\" to mkPath \"/usr1\""

  describe "pSegment" $ do
    it "creates a relative path" $ do
      let result = pSegment "home"
      result.segments `shouldBe` ["home"]
    it "errors on empty segments" $ do
      evaluate (pSegment "")
        `shouldThrow` errorCall "Cannot create a path segment from an empty string"
    it "errors on segments with slashes" $ do
      evaluate (pSegment "with/slash")
        `shouldThrow` errorCall "Path segments cannot contain slashes: \"with/slash\""

  describe "mkPath" $ do
    describe "for absolute paths" $ do
      it "creates absolute path from absolute string" $
        (mkPath "/usr/local/bin" :: Path Abs) `shouldBe` (pRoot </> "usr" </> "local" </> "bin")
      it "creates absolute path from root" $
        (mkPath "/" :: Path Abs) `shouldBe` pRoot
      it "errors on relative paths" $ do
        evaluate (mkPath "relative" :: Path Abs)
          `shouldThrow` errorCall "Path is not absolute: \"relative\""

    describe "for relative paths" $ do
      it "creates relative path from relative string" $
        (mkPath "local/bin" :: Path Rel) `shouldBe` (pSegment "local" </> "bin")
      it "creates relative path from single segment" $
        (mkPath "usr" :: Path Rel) `shouldBe` pSegment "usr"
      for_ [".", "", "./."] $ \current -> do
        it ("creates current directory path from " <> show current) $ do
          let result = mkPath current :: Path AnyAnchor
          pAbs result `shouldBe` Nothing
          result.segments `shouldBe` []
      it "errors on absolute paths" $ do
        evaluate (mkPath "/absolute" :: Path Rel)
          `shouldThrow` errorCall "Path is not relative: \"/absolute\""

    describe "for AnyAnchor paths" $ do
      it "creates absolute path from absolute string" $ do
        let result = mkPath "/usr/local" :: Path AnyAnchor
        pAbs result `shouldBe` Just (pRoot </> "usr" </> "local")
        result.segments `shouldBe` ["usr", "local"]
      it "creates relative path from relative string" $ do
        let result = mkPath "usr/local" :: Path AnyAnchor
        pAbs result `shouldBe` Nothing
        result.segments `shouldBe` ["usr", "local"]
      it "creates current directory path from ." $ do
        let result = mkPath "." :: Path AnyAnchor
        pAbs result `shouldBe` Nothing
        result.segments `shouldBe` []

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

  describe "show" $ do
    it "shows absolute paths with mkPath and quoted string" $
      show (pRoot </> "usr" </> "local") `shouldBe` "mkPath \"/usr/local\""
    it "shows relative paths with mkPath and quoted string" $
      show (pSegment "usr" </> "local") `shouldBe` "mkPath \"usr/local\""
    it "shows root as mkPath with slash" $
      show pRoot `shouldBe` "mkPath \"/\""
    it "shows single relative segment" $
      show (pSegment "usr") `shouldBe` "mkPath \"usr\""
    it "shows current directory for empty relative path" $
      show (pSegment "usr").dirname `shouldBe` "mkPath \".\""

  describe "pParent" $ do
    it "returns Nothing for root absolute path" $
      pParent pRoot `shouldBe` Nothing
    it "returns parent for single segment absolute path" $ do
      let result = pParent (pRoot </> "usr")
      result `shouldBe` Just pRoot
    it "returns parent for single segment relative path" $ do
      let result = pParent (pSegment "usr")
      (result >>= \p -> Just p.segments) `shouldBe` Just ([] :: [Text])
    it "returns parent for multi-segment absolute path" $ do
      let result = pParent (pRoot </> "usr" </> "local" </> "bin")
      result `shouldBe` Just (pRoot </> "usr" </> "local")
    it "returns parent for multi-segment relative path" $ do
      let result = pParent (pSegment "usr" </> "local" </> "bin")
      result `shouldBe` Just (pSegment "usr" </> "local")
    it "returns parent for two-segment relative path" $ do
      let result = pParent (pSegment "usr" </> "local")
      (result >>= \p -> Just p.segments) `shouldBe` Just (["usr"] :: [Text])
