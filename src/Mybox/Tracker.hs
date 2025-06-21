{-# LANGUAGE TypeFamilies #-}

module Mybox.Tracker
  ( Tracker
  , TrackerSession
  , PackageTracker
  , TrackedFile(..)
  , TrackerState(..)
  , trkSession
  , trkPackage
  , trkSkip
  , trkAdd
  , nullPackageTracker
  , stateTracker
  ) where

import qualified Data.Set                     as Set

import           Effectful.Dispatch.Dynamic
import           Effectful.State.Static.Local

import           Mybox.Package.Name
import           Mybox.Prelude

data PackageTracker :: Effect where
  TrkAdd :: Text -> PackageTracker m ()

type instance DispatchOf PackageTracker = Dynamic

trkAdd :: (PackageTracker :> es) => Text -> Eff es ()
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
     PackageName p
  => p
  -> Eff (PackageTracker : es) a
  -> Eff (TrackerSession : es) a
trkPackage pkg = interpret_ (\(TrkAdd file) -> send (TrkAdd_ pkg file)) . inject

data TrackedFile = TrackedFile
  { tfName :: !Text
  , tfPath :: !Text
  } deriving (Ord, Eq, Show)

tfMake :: PackageName p => p -> Text -> TrackedFile
tfMake = TrackedFile . pkgName

tfBelongsTo :: PackageName p => p -> TrackedFile -> Bool
tfBelongsTo pkg tf = pkgName pkg == tfName tf

data Tracker :: Effect where
  TrkGet :: Tracker m (Set TrackedFile) -- ^ Get all currently tracked files
  TrkSet :: Set TrackedFile -> Tracker m () -- ^ Set all tracked files
  TrkRemove :: Text -> Tracker m () -- ^ Remove file from tracker and disk

type instance DispatchOf Tracker = Dynamic

trkSession :: forall es a. Eff (TrackerSession : es) a -> Eff (Tracker : es) a
trkSession act = do
  ts0 <- send TrkGet
  (r, ts1) <-
    reinterpret_
      (runState mempty)
      (\case
         TrkAdd_ pkg file -> modify $ Set.insert $ tfMake pkg file
         TrkSkip pkg -> modify $ Set.union $ Set.filter (tfBelongsTo pkg) ts0)
      $ inject act
  let deletedFiles = (Set.difference `on` Set.map tfPath) ts0 ts1
  for_ deletedFiles $ send . TrkRemove
  send $ TrkSet ts1
  pure r

data TrackerState = TrackerState
  { tsTracked :: !(Set TrackedFile)
  , tsDeleted :: !(Set Text)
  } deriving (Ord, Eq, Show)

stateTracker ::
     Set TrackedFile -> Eff (Tracker : es) a -> Eff es (a, TrackerState)
stateTracker tfs =
  reinterpret_ (runState $ TrackerState tfs mempty) $ \case
    TrkGet -> gets tsTracked
    TrkSet tfs' -> modify $ \s -> s {tsTracked = tfs'}
    TrkRemove file ->
      modify $ \s -> s {tsDeleted = Set.insert file (tsDeleted s)}
