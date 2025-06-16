module Mybox.Prelude
  ( module Control.Exception.Safe
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.Foldable
  , module Data.Maybe
  , (&)
  , NonEmpty(..)
  , ExitCode(..)
  , Text
  , (</>)
  ) where

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Function          ((&))
import           Data.List.NonEmpty     (NonEmpty (..))
import           Data.Maybe
import           Data.Text              (Text)
import           System.Exit            (ExitCode (..))

(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b
