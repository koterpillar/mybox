module Mybox.Tracker (
  Tracker,
  TrackerState (..),
  tsDeleted,
  trkSkip,
  trkAdd,
  nullTracker,
  stateTracker,
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

data TrackerState = TrackerState
  { previous :: !TrackedFiles
  , current :: !TrackedFiles
  }

tsAdd :: PackageName p => p -> Set (Path Abs) -> TrackerState -> TrackerState
tsAdd pkg files ts = ts{current = Map.insertWith Set.union pkg.name files ts.current}

tsDeleted :: TrackerState -> Set (Path Abs)
tsDeleted ts = (Set.difference `on` allPaths) ts.previous ts.current
 where
  allPaths = Set.unions . Map.elems

modifyMVarPure :: Concurrent :> es => MVar a -> (a -> a) -> Eff es ()
modifyMVarPure v f = modifyMVar_ v $ pure . f

trkSession :: Concurrent :> es => MVar TrackerState -> Eff (Tracker : es) a -> Eff es a
trkSession ts act = do
  before <- (.previous) <$> readMVar ts
  interpretWith_
    (inject act)
    $ \case
      TrkAdd pkg file -> modifyMVarPure ts $ tsAdd pkg $ Set.singleton file
      TrkSkip pkg -> modifyMVarPure ts $ tsAdd pkg $ fromMaybe mempty $ Map.lookup pkg.name before

stateTracker :: Concurrent :> es => TrackedFiles -> Eff (Tracker : es) a -> Eff es (a, TrackerState)
stateTracker before act = do
  ts <- newMVar $ TrackerState before mempty
  r <- trkSession ts act
  tracked <- takeMVar ts
  pure (r, tracked)

nullTracker :: Concurrent :> es => Eff (Tracker : es) a -> Eff es a
nullTracker = fmap fst . stateTracker mempty

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
  (r, ts) <- stateTracker before act
  forM_ (tsDeleted ts) $ drvRm
  drvWriteBinaryFile stateFile $
    encode $
      TrackerStateFile ts.current
  pure r
