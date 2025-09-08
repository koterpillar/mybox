module Mybox.Tracker (
  Tracker,
  TrackerState (..),
  trkSkip,
  trkAdd,
  trkSession,
  nullTracker,
  drvTracker,
) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Effectful.Concurrent.MVar
import Effectful.Dispatch.Dynamic

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Name
import Mybox.Prelude

data Tracker :: Effect where
  TrkSkip :: PackageName p => p -> Tracker m ()
  TrkAdd :: PackageName p => p -> Path Abs -> Tracker m ()

type instance DispatchOf Tracker = Dynamic

trkSkip :: (PackageName p, Tracker :> es) => p -> Eff es ()
trkSkip = send . TrkSkip

trkAdd :: (PackageName p, Tracker :> es) => p -> Path Abs -> Eff es ()
trkAdd pkg file = send $ TrkAdd pkg file

type TrackedFiles = Map Text (Set (Path Abs))

tsAdd :: PackageName p => p -> Set (Path Abs) -> TrackedFiles -> TrackedFiles
tsAdd pkg = Map.insertWith Set.union pkg.name

tsDiff :: TrackedFiles -> TrackedFiles -> Set (Path Abs)
tsDiff = Set.difference `on` Set.unions . Map.elems

data TrackerState = TrackerState
  { tracked :: !TrackedFiles
  , deleted :: !(Set (Path Abs))
  }
  deriving (Eq, Ord, Show)

modifyMVarPure :: Concurrent :> es => MVar a -> (a -> a) -> Eff es ()
modifyMVarPure v f = modifyMVar_ v $ pure . f

trkSession :: Concurrent :> es => TrackedFiles -> Eff (Tracker : es) a -> Eff es (a, TrackerState)
trkSession before act = do
  ts <- newMVar mempty
  r <-
    interpretWith_
      (inject act)
      $ \case
        TrkAdd pkg file -> modifyMVarPure ts $ tsAdd pkg $ Set.singleton file
        TrkSkip pkg -> modifyMVarPure ts $ tsAdd pkg $ fromMaybe mempty $ Map.lookup pkg.name before
  tracked <- takeMVar ts
  let deleted = tsDiff before tracked
  pure $ (r, TrackerState{..})

nullTracker :: Concurrent :> es => Eff (Tracker : es) a -> Eff es a
nullTracker = fmap fst . trkSession mempty

newtype TrackerStateFile = TrackerStateFile
  { trackedFiles :: TrackedFiles
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TrackerStateFile

instance ToJSON TrackerStateFile where
  toEncoding = genericToEncoding defaultOptions

drvTracker :: (Anchor a, Concurrent :> es, Driver :> es) => Path a -> Eff (Tracker : es) r -> Eff es r
drvTracker stateFile act = do
  exists <- drvIsFile stateFile
  before <-
    if exists
      then
        fmap (.trackedFiles) $
          drvReadFile stateFile >>= jsonDecode @TrackerStateFile "tracker state"
      else pure mempty
  (r, after) <- trkSession before act
  forM_ after.deleted $ drvRm
  drvWriteBinaryFile stateFile $
    encode $
      TrackerStateFile after.tracked
  pure r
