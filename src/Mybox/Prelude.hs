module Mybox.Prelude (
  module Control.Exception.Safe,
  module Control.Monad,
  module Data.Either,
  module Data.Foldable,
  module Data.Maybe,
  module Effectful,
  module Effectful.Exception,
  module GHC.Generics,
  module Mybox.Path,
  (<|>),
  (&),
  for,
  on,
  uncons,
  unlessM,
  unsnoc,
  whenM,
  ExitCode (..),
  HasCallStack,
  HasField (..),
  Map,
  NonEmpty (..),
  Set,
  Text,
  terror,
  fromMaybeOrM,
  fromMaybeOrMM,
  throwLeft,
) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad
import Control.Monad.Extra (unlessM, whenM)
import Data.Either
import Data.Foldable
import Data.Function (on, (&))
import Data.List (uncons, unsnoc)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Effectful
import Effectful.Exception (Exception, bracket, bracket_, finally)
import GHC.Generics (Generic, Generically (..))
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

throwLeft :: MonadThrow m => Either String a -> m a
throwLeft = either throwString pure
