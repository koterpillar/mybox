{-# LANGUAGE TypeFamilies #-}

module Mybox.Package.Queue.Effect (InstallQueue (..)) where

import Mybox.Prelude

data InstallQueue :: Effect where
  IsInstalled :: Text -> InstallQueue m Bool
  MarkInstalled :: Text -> InstallQueue m ()

type instance DispatchOf InstallQueue = Dynamic
