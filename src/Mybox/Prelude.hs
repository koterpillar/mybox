module Mybox.Prelude (
  module Control.Monad,
  module Data.Either,
  module Data.Foldable,
  module Data.Maybe,
  module Effectful,
  module Mybox.Path,
  (<|>),
  (&),
  for,
  on,
  unlessM,
  whenM,
  ExitCode (..),
  Generic,
  HasField (..),
  Map,
  NonEmpty (..),
  Set,
  Text,
  terror,
  fromMaybeOrM,
  fromMaybeOrMM,
) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Extra (unlessM, whenM)
import Data.Either
import Data.Foldable
import Data.Function (on, (&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
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

fromMaybeOrM :: Applicative m => Maybe a -> m a -> m a
fromMaybeOrM value nothingAction = maybe nothingAction pure value

infixr 9 `fromMaybeOrM`

fromMaybeOrMM :: Monad m => m (Maybe a) -> m a -> m a
fromMaybeOrMM action nothingAction = action >>= flip fromMaybeOrM nothingAction

infixr 9 `fromMaybeOrMM`
