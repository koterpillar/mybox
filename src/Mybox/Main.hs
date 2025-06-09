module Mybox.Main
  ( main
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (runReaderT)

import          Data.List.NonEmpty     (NonEmpty (..))

import           Mybox.Driver

import           System.Exit

main :: IO ()
main = do
  let driver = localDriver
  flip runReaderT driver $ do
    drvRun $ "echo" :| ["Not implemented yet."]
    liftIO exitFailure
