module Mybox.TrackerSpec where

import Data.Map qualified as Map
import Data.Set qualified as Set

import Mybox.Driver
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

newtype DummyPackage
  = DummyPackage {name :: Text}
  deriving (Eq, Ord, Show)

spec :: Spec
spec = do
  it "adds new files, removes stale files, skips packages" $ do
    let mkAbs :: Text -> Path Abs
        mkAbs t = pRoot </> t
    let mkTrk p fs = (p, Set.fromList $ map mkAbs fs)
    let filesBefore =
          Map.fromList
            [ mkTrk "pkg1" ["common-file", "pkg1-file"]
            , mkTrk "pkg2" ["common-file", "pkg2-file"]
            , mkTrk "pkg3" ["common-file", "pkg3-file"]
            , mkTrk "pkg4" ["common-file", "pkg4-file"]
            ]
    ((), ts) <-
      stateTracker filesBefore $
        do
          let pkg1 = DummyPackage "pkg1"
          let pkg2 = DummyPackage "pkg2"
          let pkg3 = DummyPackage "pkg3"
          trkAdd pkg1 $ mkAbs "common-file"
          trkAdd pkg1 $ mkAbs "pkg1-file"
          trkSkip pkg2
          trkAdd pkg3 $ mkAbs "common-file"
          trkAdd pkg3 $ mkAbs "pkg3-file-new"
    ts.current
      `shouldBe` Map.fromList
        [ mkTrk "pkg1" ["common-file", "pkg1-file"]
        , mkTrk "pkg2" ["common-file", "pkg2-file"]
        , mkTrk "pkg3" ["common-file", "pkg3-file-new"]
        ]
    tsDeleted ts `shouldBe` Set.fromList (map mkAbs ["pkg3-file", "pkg4-file"])
  describe "drvTracker" $ do
    it "gets, sets state and removes files" $
      drvTempDir $ \dir -> do
        let testFile = dir </> "file1"
        drvWriteFile testFile "content"
        let state = dir </> "state.json"
        let pkg = DummyPackage "pkg1"
        -- Record the test file as owned by the package
        drvTracker state $ trkAdd pkg testFile
        -- State should be written out
        drvIsFile state >>= (`shouldBe` True)
        drvReadFile state
          >>= (`shouldBe` ("{\"trackedFiles\":{\"pkg1\":[\"" <> testFile.text <> "\"]}}"))
        -- Now record no files
        drvTracker state $ pure ()
        -- State is still there but empty
        drvIsFile state >>= (`shouldBe` True)
        drvReadFile state >>= (`shouldBe` "{\"trackedFiles\":{}}")
        -- Test file should be deleted
        drvIsFile testFile >>= (`shouldBe` False)
    it "reads and removes previously saved files" $
      drvTempDir $ \dir -> do
        let testFile = dir </> "file1"
        drvWriteFile testFile "content"
        -- Assume previous run tracked the test file
        let state = dir </> "state.json"
        drvWriteFile state $ "{\"trackedFiles\":{\"pkg1\":[\"" <> testFile.text <> "\"]}}"
        -- Record no files
        drvTracker state $ pure ()
        -- State is still there but empty
        drvIsFile state >>= (`shouldBe` True)
        drvReadFile state >>= (`shouldBe` "{\"trackedFiles\":{}}")
        -- Test file should be deleted
        drvIsFile testFile >>= (`shouldBe` False)
