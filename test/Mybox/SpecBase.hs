module Mybox.SpecBase where

import           Control.Monad.Reader (runReaderT)

import qualified Data.Text            as Text

import           Mybox.Driver.Class
import           Mybox.Driver.IO
import           Mybox.Prelude

import           System.Environment

withDriver :: (IODriver -> IO ()) -> IO ()
withDriver act = do
  image_ <- lookupEnv "DOCKER_IMAGE"
  case image_ of
    Nothing    -> testHostDriver act
    Just image -> dockerDriver (Text.pack image) act

run :: IODriver -> (forall m. (MonadDriver m, MonadIO m) => m a) -> IO a
run drv action = runReaderT action drv
