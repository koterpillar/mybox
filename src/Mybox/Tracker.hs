{-# LANGUAGE TypeFamilies #-}

module Mybox.Tracker (
  Tracker,
  TrackerSession,
  TrackedFile (..),
  TrackerState (..),
  trkSession,
  trkSkip,
  trkAdd,
  nullTrackerSession,
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
  TrkAdd :: PackageName p => p -> Text -> TrackerSession m ()

type instance DispatchOf TrackerSession = Dynamic

trkSkip :: (PackageName p, TrackerSession :> es) => p -> Eff es ()
trkSkip = send . TrkSkip

trkAdd :: (PackageName p, TrackerSession :> es) => p -> Text -> Eff es ()
trkAdd pkg file = send $ TrkAdd pkg file

nullTrackerSession :: Eff (TrackerSession : es) a -> Eff es a
nullTrackerSession =
  interpret_ $ \case
    TrkAdd _ _ -> pure ()
    TrkSkip _ -> pure ()

data TrackedFile = TrackedFile
  { name :: !Text
  , path :: !Text
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TrackedFile

instance ToJSON TrackedFile where
  toEncoding = genericToEncoding defaultOptions

tfMake :: PackageName p => p -> Text -> TrackedFile
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
    Text ->
    -- | Remove file from disk
    Tracker m ()

type instance DispatchOf Tracker = Dynamic

trkSession :: Tracker :> es => Eff (TrackerSession : es) a -> Eff es a
trkSession act = do
  ts0 <- send TrkGet
  (r, ts1) <-
    reinterpretWith_
      (runState mempty)
      (inject act)
      $ \case
        TrkAdd pkg file -> modify $ Set.insert $ tfMake pkg file
        TrkSkip pkg -> modify $ Set.union $ Set.filter (tfBelongsTo pkg) ts0
  let deletedFiles = (Set.difference `on` Set.map (.path)) ts0 ts1
  for_ deletedFiles $ send . TrkRemove
  send $ TrkSet ts1
  pure r

data TrackerState = TrackerState
  { tracked :: !(Set TrackedFile)
  , deleted :: !(Set Text)
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

drvTracker :: Driver :> es => Text -> Eff (Tracker : es) a -> Eff es a
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
      drvWriteFile stateFile $
        jsonEncode $
          TrackedFiles tfs
    TrkRemove file -> drvRm file
