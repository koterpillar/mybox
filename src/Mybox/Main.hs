module Mybox.Main
  ( main
  ) where

import           Control.Monad.Reader (runReaderT)

import           Mybox.Driver
import           Mybox.Prelude

import           System.Exit

main :: IO ()
main = do
  let driver = localDriver
  flip runReaderT driver $ do
    drvRun $ "echo" :| ["Not implemented yet."]
    liftIO exitFailure
