module Mybox.Package.Queue.Effect (InstallQueue (..)) where

import Mybox.Prelude

data InstallQueue :: Effect where
  Enqueue :: [(Text, m ())] -> InstallQueue m ()

type instance DispatchOf InstallQueue = Dynamic
