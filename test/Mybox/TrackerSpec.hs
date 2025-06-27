module Mybox.TrackerSpec where

import qualified Data.Set           as Set

import           Mybox.Prelude
import           Mybox.SpecBase
import           Mybox.Tracker

newtype DummyPackage =
  DummyPackage { name :: Text }
  deriving (Eq, Ord, Show)

spec :: Spec
spec =
  around withIOEnv $ do
    it "adds new files, removes stale files, skips packages" $ do
      let filesBefore =
            Set.fromList
              $ map (TrackedFile "pkg1") ["common-file", "pkg1-file"]
                  <> map (TrackedFile "pkg2") ["common-file", "pkg2-file"]
                  <> map (TrackedFile "pkg3") ["common-file", "pkg3-file"]
                  <> map (TrackedFile "pkg4") ["common-file", "pkg4-file"]
      ((), TrackerState filesAfter deleted) <-
        stateTracker filesBefore
          $ trkSession
          $ do
              trkPackage (DummyPackage "pkg1") $ do
                trkAdd "common-file"
                trkAdd "pkg1-file"
              trkSkip (DummyPackage "pkg2")
              trkPackage (DummyPackage "pkg3") $ do
                trkAdd "common-file"
                trkAdd "pkg3-file-new"
      filesAfter
        `shouldBe` Set.fromList
                     (map (TrackedFile "pkg1") ["common-file", "pkg1-file"]
                        <> map (TrackedFile "pkg2") ["common-file", "pkg2-file"]
                        <> map
                             (TrackedFile "pkg3")
                             ["common-file", "pkg3-file-new"])
      deleted `shouldBe` Set.fromList ["pkg3-file", "pkg4-file"]
