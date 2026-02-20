module Mybox.Tracker.Internal where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Effectful.Dispatch.Dynamic

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Name
import Mybox.Prelude

data Tracker :: Effect where
  TrkSkip :: PackageName p => p -> Tracker m ()
  TrkAdd :: PackageName p => p -> Path Abs -> Tracker m ()

type instance DispatchOf Tracker = Dynamic

trkTry :: (PackageName p, Tracker :> es) => p -> Eff es r -> Eff es r
trkTry pkg act = act `onException` trkSkip pkg

trkSkip :: (PackageName p, Tracker :> es) => p -> Eff es ()
trkSkip = send . TrkSkip

trkAdd :: (PackageName p, Tracker :> es) => p -> Path Abs -> Eff es ()
trkAdd pkg file = send $ TrkAdd pkg file

type TrackedFiles = Map (Path Abs) (Set Text)

data TrackerState = TrackerState
  { added :: !TrackedFiles
  , skipped :: !(Set Text)
  }

tsSkip :: PackageName p => p -> TrackerState -> TrackerState
tsSkip pkg ts = ts{skipped = Set.insert pkg.name ts.skipped}

tsAdd :: PackageName p => p -> Path Abs -> TrackerState -> TrackerState
tsAdd pkg f ts = ts{added = Map.insertWith Set.union f (Set.singleton pkg.name) ts.added}

data TrackResult = TrackResult
  { deleted :: Set (Path Abs)
  , state :: !TrackedFiles
  }
  deriving (Generic)
  deriving (Monoid, Semigroup) via (Generically TrackResult)

tsKeep :: TrackerState -> Path Abs -> Set Text -> TrackResult
tsKeep ts f pkgs = TrackResult{..}
 where
  deleted = if shouldDelete then Set.singleton f else mempty
  state = if Set.null keptBy then Map.empty else Map.singleton f keptBy
  keptBy = addedBy <> skippedBy
  skippedBy = Set.intersection pkgs ts.skipped
  addedBy = fromMaybe Set.empty $ Map.lookup f ts.added
  shouldDelete = Set.null skippedBy && not descendantsKept
  descendantsKept = any (pUnder f) $ Map.keys ts.added

tsResult :: TrackedFiles -> TrackerState -> TrackResult
tsResult before ts =
  mconcat [tsKeep ts f pkgs | (f, pkgs) <- Map.toList before]
    <> mempty{state = Map.difference ts.added before}

trkSession :: Concurrent :> es => MVar TrackerState -> Eff (Tracker : es) a -> Eff es a
trkSession ts = interpret_ $
  \case
    TrkAdd pkg file -> modifyMVarPure ts $ tsAdd pkg file
    TrkSkip pkg -> modifyMVarPure ts $ tsSkip pkg

stateTracker :: Concurrent :> es => TrackedFiles -> Eff (Tracker : es) a -> Eff es (a, TrackResult)
stateTracker before act = do
  ts <- newMVar $ TrackerState{added = mempty, skipped = mempty}
  r <- trkSession ts act
  tracked <- takeMVar ts
  pure (r, tsResult before tracked)

nullTracker :: Concurrent :> es => Eff (Tracker : es) a -> Eff es a
nullTracker = fmap fst . stateTracker mempty

newtype TrackerStateFile = TrackerStateFile
  { trackedFiles :: TrackedFiles
  }
  deriving (Generic)

instance FromJSON TrackerStateFile

instance ToJSON TrackerStateFile where
  toEncoding = genericToEncoding defaultOptions

-- FIXME: store whether files were created with sudo?
rmWithRoot :: Driver :> es => Path Abs -> Eff es ()
rmWithRoot p = do
  sudo' <- mkSudo
  let m
        | pUnder (pRoot </> "etc") p = sudo'
        | pUnder (pRoot </> "root") p = sudo'
        | pUnder (pRoot </> "var" </> "root") p = sudo' -- macOS root home
        | otherwise = id
  modifyDriver m $ drvRm p

drvTracker :: (Anchor a, Concurrent :> es, Driver :> es) => Path a -> Eff (Tracker : es) r -> Eff es r
drvTracker stateFile act = do
  exists <- drvIsFile stateFile
  before <-
    if exists
      then
        fmap (.trackedFiles) $
          drvReadFile stateFile >>= jsonDecode @TrackerStateFile "tracker state"
      else pure mempty
  (r, after) <- stateTracker before act
  forM_ after.deleted $ rmWithRoot
  drvWriteBinaryFile stateFile $
    encode $
      TrackerStateFile after.state
  pure r
