module Mybox.Package.Queue.Effect (InstallQueue (..), IQResult) where

import Mybox.Prelude

type IQResult = Either SomeException ()

data InstallQueue :: Effect where
  Enqueue :: Traversable f => f (Text, m ()) -> InstallQueue m (f IQResult)

type instance DispatchOf InstallQueue = Dynamic
