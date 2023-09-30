{-# LANGUAGE GADTs #-}

module Driver.Types
  ( Driver(..)
  , drvRun
  , drvRunOK
  , drvRunOutput
  , OutStream(..)
  , RunOptions(..)
  , roPrependArgs
  , RunResult(..)
  , runOK
  , drvRunOptions
  , RunException(..)
  , NonEmpty(..)
  ) where

import           Control.Exception.Base

import           Control.Monad          (void)

import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.List.NonEmpty     as NonEmpty

import           Data.Text              (Text)

import           System.Process.Typed   hiding (Inherit)

data OutStream output where
  Capture :: OutStream Text
  Silent :: OutStream ()
  Inherit :: OutStream ()

data RunResult output =
  RunResult
    { runExitCode :: ExitCode
    , runOutput   :: output
    }
  deriving (Eq, Show)

runOK :: RunResult output -> Bool
runOK RunResult {..} = runExitCode == ExitSuccess

data RunOptions output =
  RunOptions
    { roArgs   :: NonEmpty Text
    , roCheck  :: Bool
    , roInput  :: Maybe Text
    , roOutput :: OutStream output
    , roErrors :: OutStream ()
    }

roDefaults :: NonEmpty Text -> RunOptions ()
roDefaults args =
  RunOptions
    { roArgs = args
    , roCheck = True
    , roInput = Nothing
    , roOutput = Silent
    , roErrors = Inherit
    }

roPrependArgs :: [Text] -> RunOptions output -> RunOptions output
roPrependArgs args ro = ro {roArgs = NonEmpty.prependList args (roArgs ro)}

newtype Driver =
  Driver
    { drvRun_ :: forall output. RunOptions output -> IO (RunResult output)
    }

drvRunOptions :: Driver -> RunOptions output -> IO (RunResult output)
drvRunOptions Driver {..} = drvRun_

drvRun :: NonEmpty Text -> Driver -> IO ()
drvRun args driver = void $ drvRunOptions driver $ roDefaults args

drvRunOK :: NonEmpty Text -> Driver -> IO Bool
drvRunOK args driver = runOK <$> drvRunOptions driver ro
  where
    ro = (roDefaults args) {roCheck = False}

drvRunOutput :: NonEmpty Text -> Driver -> IO Text
drvRunOutput args driver = runOutput <$> drvRunOptions driver ro
  where
    ro = (roDefaults args) {roOutput = Capture, roErrors = Silent}

newtype RunException =
  RunException (NonEmpty Text)
  deriving (Eq, Show)

instance Exception RunException
