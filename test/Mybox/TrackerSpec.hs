module Mybox.TrackerSpec where

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
  describe "trkSession" $ do
    let mkAbs :: Text -> Path Abs
        mkAbs t = pRoot </> t
    let mkTrk p = TrackedFile p . mkAbs
    let filesBefore =
          Set.fromList $
            map (mkTrk "pkg1") ["common-file", "pkg1-file"]
              <> map (mkTrk "pkg2") ["common-file", "pkg2-file"]
              <> map (mkTrk "pkg3") ["common-file", "pkg3-file"]
              <> map (mkTrk "pkg4") ["common-file", "pkg4-file"]
    let act :: TrackerSession :> es => Eff es ()
        act = do
          let pkg1 = DummyPackage "pkg1"
          let pkg2 = DummyPackage "pkg2"
          let pkg3 = DummyPackage "pkg3"
          trkAdd pkg1 $ mkAbs "common-file"
          trkAdd pkg1 $ mkAbs "pkg1-file"
          trkSkip pkg2
          trkAdd pkg3 $ mkAbs "pkg3-file-new"
    describe "total" $
      it "adds new files, removes stale files and orphans, skips packages" $ do
        ((), TrackerState filesAfter deleted) <- stateTracker filesBefore $ trkSession True act
        filesAfter
          `shouldBe` Set.fromList
            ( map (mkTrk "pkg1") ["common-file", "pkg1-file"]
                <> map (mkTrk "pkg2") ["common-file", "pkg2-file"]
                <> map (mkTrk "pkg3") ["pkg3-file-new"]
            )
        deleted `shouldBe` Set.fromList (map mkAbs ["pkg3-file", "pkg4-file"])
    describe "partial" $ do
      it "adds new files, removes stale files, skips packages, keeps orphans" $ do
        ((), TrackerState filesAfter deleted) <- stateTracker filesBefore $ trkSession False act
        filesAfter
          `shouldBe` Set.fromList
            ( map (mkTrk "pkg1") ["common-file", "pkg1-file"]
                <> map (mkTrk "pkg2") ["common-file", "pkg2-file"]
                <> map (mkTrk "pkg3") ["pkg3-file-new"]
                <> map (mkTrk "pkg4") ["common-file", "pkg4-file"]
            )
        deleted `shouldBe` Set.fromList (map mkAbs ["pkg3-file"])
  describe "drvTracker" $ do
    it "gets, sets state and removes files" $
      drvTempDir $ \dir -> do
        let testFile = dir </> "file1"
        drvWriteFile testFile "content"
        let state = dir </> "state.json"
        let pkg = DummyPackage "pkg1"
        -- Record the test file as owned by the package
        drvTracker state $ trkSession True $ trkAdd pkg testFile
        -- State should be written out
        drvIsFile state >>= (`shouldBe` True)
        -- Now record no files
        drvTracker state $ trkSession True $ pure ()
        -- State is still there but empty
        drvIsFile state >>= (`shouldBe` True)
        drvReadFile state >>= (`shouldBe` "{\"trackedFiles\":[]}")
        -- Test file should be deleted
        drvIsFile testFile >>= (`shouldBe` False)
