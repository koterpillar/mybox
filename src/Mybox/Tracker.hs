{-# LANGUAGE TypeFamilies #-}

module Mybox.Tracker (
  Tracker,
  TrackerSession,
  PackageTracker,
  TrackedFile (..),
  TrackerState (..),
  trkSession,
  trkPackage,
  trkSkip,
  trkAdd,
  nullPackageTracker,
  stateTracker,
  drvTracker,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local

import Mybox.Driver
import Mybox.Package.Name
import Mybox.Prelude

data PackageTracker :: Effect where
  TrkAdd :: Text -> PackageTracker m ()

type instance DispatchOf PackageTracker = Dynamic

trkAdd :: PackageTracker :> es => Text -> Eff es ()
trkAdd = send . TrkAdd

nullPackageTracker :: Eff (PackageTracker : es) a -> Eff es a
nullPackageTracker =
  interpret_ $ \case
    TrkAdd _ -> pure ()

data TrackerSession :: Effect where
  TrkSkip :: PackageName p => p -> TrackerSession m ()
  TrkAdd_ :: PackageName p => p -> Text -> TrackerSession m ()

type instance DispatchOf TrackerSession = Dynamic

trkSkip :: (PackageName p, TrackerSession :> es) => p -> Eff es ()
trkSkip = send . TrkSkip

trkPackage ::
  PackageName p =>
  p ->
  Eff (PackageTracker : es) a ->
  Eff (TrackerSession : es) a
trkPackage pkg = interpret_ (\(TrkAdd file) -> send (TrkAdd_ pkg file)) . inject

data TrackedFile = TrackedFile
  { name :: !Text
  , path :: !Text
  }
  deriving (Eq, Generic, Ord, Show)

instance Aeson.FromJSON TrackedFile

instance Aeson.ToJSON TrackedFile where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

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

trkSession :: forall es a. Eff (TrackerSession : es) a -> Eff (Tracker : es) a
trkSession act = do
  ts0 <- send TrkGet
  (r, ts1) <-
    reinterpret_
      (runState mempty)
      ( \case
          TrkAdd_ pkg file -> modify $ Set.insert $ tfMake pkg file
          TrkSkip pkg -> modify $ Set.union $ Set.filter (tfBelongsTo pkg) ts0
      )
      $ inject act
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
  { getTrackedFiles :: Set TrackedFile
  }
  deriving (Eq, Generic, Ord, Show)

instance Aeson.FromJSON TrackedFiles

instance Aeson.ToJSON TrackedFiles where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

drvTracker :: Driver :> es => Text -> Eff (Tracker : es) a -> Eff es a
drvTracker stateFile =
  interpret_ $ \case
    TrkGet -> do
      exists <- drvIsFile stateFile
      if exists
        then
          maybe mempty getTrackedFiles . Aeson.decodeStrictText
            <$> drvReadFile stateFile
        else pure mempty
    TrkSet tfs ->
      drvWriteFile stateFile $
        Text.decodeUtf8 $
          LBS.toStrict $
            Aeson.encode $
              TrackedFiles tfs
    TrkRemove file -> drvRm file
