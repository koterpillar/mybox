module Mybox.Prelude (
  module Control.Monad,
  module Data.Foldable,
  module Data.Maybe,
  module Effectful,
  module Mybox.Path,
  (&),
  for,
  on,
  ExitCode (..),
  Generic,
  HasField (..),
  NonEmpty (..),
  Set,
  Text,
  terror,
) where

import Control.Monad
import Data.Foldable
import Data.Function (on, (&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Effectful
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode (..))

import Mybox.Path

terror :: HasCallStack => Text -> a
terror !msg = error $ Text.unpack msg
