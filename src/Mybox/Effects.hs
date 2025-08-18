module Mybox.Effects (
  App,
) where

import Mybox.Driver
import Mybox.Package.Queue.Effect
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

type App es =
  ( Driver :> es
  , InstallQueue :> es
  , Stores :> es
  , TrackerSession :> es
  )
