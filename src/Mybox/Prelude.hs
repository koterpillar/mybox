module Mybox.Prelude (
  module Control.Monad,
  module Data.Foldable,
  module Data.Maybe,
  module Effectful,
  module Mybox.Path,
  (&),
  on,
  ExitCode (..),
  Generic,
  HasField (..),
  NonEmpty (..),
  Set,
  Text,
) where

import Control.Monad
import Data.Foldable
import Data.Function (on, (&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Effectful
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import System.Exit (ExitCode (..))

import Mybox.Path
