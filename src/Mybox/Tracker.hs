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

import           Data.Set                   (Set)

import           Effectful.Dispatch.Dynamic

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

data Tracker :: Effect where
  TrkGet :: Tracker m (Set TrackedFile)
  TrkRemember :: TrackedFile -> Tracker m ()
  TrkRemove :: Text -> Tracker m ()

type instance DispatchOf Tracker = Dynamic

trkSession :: Eff (TrackerSession : es) a -> Eff (Tracker : es) a
trkSession _ = do
  _ <- send TrkGet
  send $ TrkRemember undefined
  send $ TrkRemove undefined
  error "Not implemented"

data TrackerState = TrackerState
  { tsTracked :: !(Set TrackedFile)
  , tsDeleted :: !(Set Text)
  } deriving (Ord, Eq, Show)

stateTracker ::
     Set TrackedFile -> Eff (Tracker : es) a -> Eff es (a, TrackerState)
stateTracker = undefined
