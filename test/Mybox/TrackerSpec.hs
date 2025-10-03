module Mybox.TrackerSpec where

import Data.Map qualified as Map
import Data.Set qualified as Set

import Mybox.Driver
import Mybox.Prelude
import Mybox.Spec.Utils
import Mybox.SpecBase
import Mybox.Tracker.Internal

newtype DummyPackage
  = DummyPackage {name :: Text}

spec :: Spec
spec = do
  it "adds new files, removes stale files, skips packages" $ do
    let mkAbs :: Text -> Path Abs
        mkAbs t = pRoot </> t
    let mkTrk f ps = (mkAbs f, Set.fromList ps)
    let filesBefore =
          Map.fromList
            [ mkTrk "common-file" ["pkg1", "pkg2", "pkg3", "pkg4"]
            , mkTrk "pkg1-file" ["pkg1"]
            , mkTrk "pkg2-file" ["pkg2"]
            , mkTrk "pkg3-file" ["pkg3"]
            , mkTrk "pkg4-file" ["pkg4"]
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
    ts.state
      `shouldBe` Map.fromList
        [ mkTrk "common-file" ["pkg1", "pkg2", "pkg3"]
        , mkTrk "pkg1-file" ["pkg1"]
        , mkTrk "pkg2-file" ["pkg2"]
        , mkTrk "pkg3-file-new" ["pkg3"]
        ]
    ts.deleted `shouldBe` Set.fromList (map mkAbs ["pkg3-file", "pkg4-file"])
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
          >>= (`shouldBe` ("{\"trackedFiles\":{\"" <> testFile.text <> "\":[\"pkg1\"]}}"))
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
        drvWriteFile state $ "{\"trackedFiles\":{\"" <> testFile.text <> "\":[\"pkg1\"]}}"
        -- Record no files
        drvTracker state $ pure ()
        -- State is still there but empty
        drvIsFile state >>= (`shouldBe` True)
        drvReadFile state >>= (`shouldBe` "{\"trackedFiles\":{}}")
        -- Test file should be deleted
        drvIsFile testFile >>= (`shouldBe` False)
    onlyIf "Root tests pollute real /root and require a virtual system" virtualSystem $
      it "removes root-installed files with sudo" $
        drvTempDir $ \dir -> do
          -- Write a file as root
          rootHome <- drvHome_ "root"
          testFile <- (rootHome </>) <$> randomText "file"
          modifyDriver sudo $ drvWriteFile testFile "content"
          -- Record the file as tracked from a previous run
          let state = dir </> "state.json"
          drvWriteFile state $ "{\"trackedFiles\":{\"" <> testFile.text <> "\":[\"pkg1\"]}}"
          -- Record no files
          drvTracker state $ pure ()
          -- Test file should be deleted (checking with root as the regular user
          -- won't see it)
          modifyDriver sudo (drvIsFile testFile) >>= (`shouldBe` False)
