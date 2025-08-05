module Mybox.TrackerSpec where

import Data.Set qualified as Set

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
    let mkTrk p = TrackedFile p . mkAbs
    let filesBefore =
          Set.fromList $
            map (mkTrk "pkg1") ["common-file", "pkg1-file"]
              <> map (mkTrk "pkg2") ["common-file", "pkg2-file"]
              <> map (mkTrk "pkg3") ["common-file", "pkg3-file"]
              <> map (mkTrk "pkg4") ["common-file", "pkg4-file"]
    ((), TrackerState filesAfter deleted) <-
      stateTracker filesBefore $
        trkSession $
          do
            let pkg1 = DummyPackage "pkg1"
            let pkg2 = DummyPackage "pkg2"
            let pkg3 = DummyPackage "pkg3"
            trkAdd pkg1 $ mkAbs "common-file"
            trkAdd pkg1 $ mkAbs "pkg1-file"
            trkSkip pkg2
            trkAdd pkg3 $ mkAbs "common-file"
            trkAdd pkg3 $ mkAbs "pkg3-file-new"
    filesAfter
      `shouldBe` Set.fromList
        ( map (mkTrk "pkg1") ["common-file", "pkg1-file"]
            <> map (mkTrk "pkg2") ["common-file", "pkg2-file"]
            <> map
              (mkTrk "pkg3")
              ["common-file", "pkg3-file-new"]
        )
    deleted `shouldBe` Set.fromList (map mkAbs ["pkg3-file", "pkg4-file"])
