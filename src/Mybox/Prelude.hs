module Mybox.Prelude
  ( module Control.Monad
  , module Data.Foldable
  , module Data.Maybe
  , module Effectful
  , (&)
  , on
  , NonEmpty(..)
  , ExitCode(..)
  , Text
  , (</>)
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.Function      (on, (&))
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe
import           Data.Text          (Text)
import           Effectful
import           System.Exit        (ExitCode (..))

(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b
