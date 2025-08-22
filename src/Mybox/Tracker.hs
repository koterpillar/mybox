module Mybox.Tracker (
  Tracker,
  TrackerSession,
  TrackedFile (..),
  TrackerState (..),
  trkSession,
  trkSkip,
  trkAdd,
  nullTrackerSession,
  nullTracker,
  stateTracker,
  drvTracker,
) where

import Data.Set qualified as Set
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Name
import Mybox.Prelude

data TrackerSession :: Effect where
  TrkSkip :: PackageName p => p -> TrackerSession m ()
  TrkAdd :: PackageName p => p -> Path Abs -> TrackerSession m ()

type instance DispatchOf TrackerSession = Dynamic

trkSkip :: (PackageName p, TrackerSession :> es) => p -> Eff es ()
trkSkip = send . TrkSkip

trkAdd :: (PackageName p, TrackerSession :> es) => p -> Path Abs -> Eff es ()
trkAdd pkg file = send $ TrkAdd pkg file

data TrackedFile = TrackedFile
  { name :: !Text
  , path :: !(Path Abs)
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TrackedFile

instance ToJSON TrackedFile where
  toEncoding = genericToEncoding defaultOptions

tfMake :: PackageName p => p -> Path Abs -> TrackedFile
tfMake = TrackedFile . (.name)

tfBelongsTo :: PackageName p => p -> TrackedFile -> Bool
tfBelongsTo pkg tf = pkg.name == tf.name

data Tracker :: Effect where
  TrkGet ::
    -- | Get all currently tracked files
    Tracker m (Set TrackedFile)
  TrkSet ::
    Set TrackedFile ->
    -- | Set all tracked files
    Tracker m ()
  TrkRemove ::
    Path Abs ->
    -- | Remove file from disk
    Tracker m ()

type instance DispatchOf Tracker = Dynamic

-- | Given sets of tracked files before and after, which files should be deleted?
toDelete :: Bool -> Set TrackedFile -> Set TrackedFile -> Set (Path Abs)
toDelete True old new = (Set.difference `on` Set.map (.path)) old new
toDelete False old new =
  Set.map (.path) $ Set.filter removedFromExistingPackage old
 where
  removedFromExistingPackage tf
    | not (Set.member tf.name newPackages) = False
    | Set.member tf.path newPaths = False
    | otherwise = True
  newPackages = Set.map (.name) new
  newPaths = Set.map (.path) new

-- | Given sets of tracked files before and after, what to record as the action result?
trackResult :: Bool -> Set TrackedFile -> Set TrackedFile -> Set TrackedFile
trackResult True _ new = new
trackResult False old new = Set.union new $ Set.filter fromUnrelatedPackage old
 where
  fromUnrelatedPackage tf = Set.notMember tf.name $ Set.map (.name) new

-- | Record the files from packages installed by the action and remove the
-- orphaned ones.
trkSession ::
  Tracker :> es =>
  -- | Whether to remove files from packages not installed/skipped during the action
  Bool ->
  -- | Action to run
  Eff (TrackerSession : es) a ->
  Eff es a
trkSession total act = do
  tsOld <- send TrkGet
  (r, tsNew) <-
    reinterpretWith_
      (runState mempty)
      (inject act)
      $ \case
        TrkAdd pkg file -> modify $ Set.insert $ tfMake pkg file
        TrkSkip pkg -> modify $ Set.union $ Set.filter (tfBelongsTo pkg) tsOld
  for_ (toDelete total tsOld tsNew) $ send . TrkRemove
  send $ TrkSet $ trackResult total tsOld tsNew
  pure r

nullTracker :: Eff (Tracker : es) a -> Eff es a
nullTracker = interpret_ $ \case
  TrkGet -> pure mempty
  TrkSet _ -> pure ()
  TrkRemove _ -> pure ()

nullTrackerSession :: Eff (TrackerSession : es) a -> Eff es a
nullTrackerSession = nullTracker . trkSession True . inject

data TrackerState = TrackerState
  { tracked :: !(Set TrackedFile)
  , deleted :: !(Set (Path Abs))
  }
  deriving (Eq, Ord, Show)

stateTracker ::
  Set TrackedFile -> Eff (Tracker : es) a -> Eff es (a, TrackerState)
stateTracker tfs =
  reinterpret_ (runState $ TrackerState tfs mempty) $ \case
    TrkGet -> gets @TrackerState (.tracked)
    TrkSet tfs' -> modify $ \s -> s{tracked = tfs'}
    TrkRemove file ->
      modify $ \s -> s{deleted = Set.insert file s.deleted}

newtype TrackedFiles = TrackedFiles
  { trackedFiles :: Set TrackedFile
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TrackedFiles

instance ToJSON TrackedFiles where
  toEncoding = genericToEncoding defaultOptions

drvTracker :: (Anchor a, Driver :> es) => Path a -> Eff (Tracker : es) r -> Eff es r
drvTracker stateFile =
  interpret_ $ \case
    TrkGet -> do
      exists <- drvIsFile stateFile
      if exists
        then
          maybe mempty (.trackedFiles) . jsonDecode @TrackedFiles "tracked files"
            <$> drvReadFile stateFile
        else pure mempty
    TrkSet tfs ->
      drvWriteBinaryFile stateFile $
        encode $
          TrackedFiles tfs
    TrkRemove file -> drvRm file
