{-# LANGUAGE TypeFamilies #-}

module Mybox.Tracker
  ( Tracker
  , trkSkip
  , trkAdd
  , nullTracker
  ) where

import           Effectful.Dispatch.Dynamic

import           Mybox.Package.Name
import           Mybox.Prelude

data Tracker :: Effect where
  TrkSkip :: PackageName a => a -> Tracker m ()
  TrkAdd :: PackageName a => a -> Text -> Tracker m ()

type instance DispatchOf Tracker = Dynamic

trkSkip :: (PackageName a, Tracker :> es) => a -> Eff es ()
trkSkip = send . TrkSkip

trkAdd :: (PackageName a, Tracker :> es) => a -> Text -> Eff es ()
trkAdd pkg = send . TrkAdd pkg

nullTracker :: Eff (Tracker : es) a -> Eff es a
nullTracker =
  interpret_ @Tracker $ \case
    TrkSkip _ -> pure ()
    TrkAdd _ _ -> pure ()
