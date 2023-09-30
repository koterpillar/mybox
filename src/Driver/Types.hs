{-# LANGUAGE GADTs #-}

module Driver.Types
  ( Driver(..)
  , drvModify
  , drvRun
  , drvRunOK
  , drvRunOutput
  , OutStream(..)
  , RunOptions(..)
  , drvPrependArgs
  , RunResult(..)
  , runOK
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

type DrvRun = forall output. RunOptions output -> IO (RunResult output)

newtype Driver =
  Driver
    { drvRun_ :: DrvRun
    }

drvModify :: (DrvRun -> DrvRun) -> Driver -> Driver
drvModify f (Driver run) = Driver (f run)

drvPrependArgs :: [Text] -> Driver -> Driver
drvPrependArgs args = drvModify $ \run -> run . roPrependArgs args

drvRun :: NonEmpty Text -> Driver -> IO ()
drvRun args driver = void $ drvRun_ driver $ roDefaults args

drvRunOK :: NonEmpty Text -> Driver -> IO Bool
drvRunOK args driver = runOK <$> drvRun_ driver ro
  where
    ro = (roDefaults args) {roCheck = False}

drvRunOutput :: NonEmpty Text -> Driver -> IO Text
drvRunOutput args driver = runOutput <$> drvRun_ driver ro
  where
    ro = (roDefaults args) {roOutput = Capture, roErrors = Silent}

newtype RunException =
  RunException (NonEmpty Text)
  deriving (Eq, Show)

instance Exception RunException
