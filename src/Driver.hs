{-# LANGUAGE GADTs #-}

module Driver
  ( Driver
  , drvRoot
  , drvWithRoot
  , drvRun
  , drvRunOK
  , drvRunOutput
  , RunException(..)
  , drvLocal
  , NonEmpty(..)
  ) where

import           Control.Exception.Base

import           Control.Monad          (void, when)

import           Control.Monad.State    (execState, modify)

import qualified Data.ByteString.Lazy   as LBS

import           Data.Function          ((&))

import           Data.Foldable          (for_)

import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.List.NonEmpty     as NonEmpty

import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text

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
    , roOutput :: OutStream output
    , roErrors :: OutStream ()
    , roInput  :: Maybe Text
    }

roDefaults :: NonEmpty Text -> RunOptions ()
roDefaults args =
  RunOptions
    { roArgs = args
    , roCheck = True
    , roOutput = Silent
    , roErrors = Inherit
    , roInput = Nothing
    }

roPrependArgs :: [Text] -> RunOptions output -> RunOptions output
roPrependArgs args ro = ro {roArgs = NonEmpty.prependList args (roArgs ro)}

roProc :: RunOptions output -> ProcessConfig () () ()
roProc RunOptions {..} =
  let executable :| args = roArgs
   in proc (Text.unpack executable) (map Text.unpack args)

data Driver =
  Driver
    { drvRun_ :: forall output. Bool -> RunOptions output -> IO (RunResult output)
    , drvRoot :: Bool
    }

drvWithRoot :: Bool -> Driver -> Driver
drvWithRoot root driver = driver {drvRoot = root}

drvRunO :: Driver -> RunOptions output -> IO (RunResult output)
drvRunO Driver {..} = drvRun_ drvRoot

drvRun :: NonEmpty Text -> Driver -> IO ()
drvRun args driver = void $ drvRunO driver $ roDefaults args

drvRunOK :: NonEmpty Text -> Driver -> IO Bool
drvRunOK args driver = runOK <$> drvRunO driver ro
  where
    ro = (roDefaults args) {roCheck = False}

drvRunOutput :: NonEmpty Text -> Driver -> IO Text
drvRunOutput args driver = runOutput <$> drvRunO driver ro
  where
    ro = (roDefaults args) {roOutput = Capture, roErrors = Silent}

drvLocal :: Driver
drvLocal = Driver {drvRun_ = drvLocalRun, drvRoot = False}

drvLocalRun :: Bool -> RunOptions output -> IO (RunResult output)
drvLocalRun False = drvProcessRun
drvLocalRun True  = drvProcessRun . roPrependArgs ["sudo"]

newtype RunException =
  RunException (NonEmpty Text)
  deriving (Eq, Show)

instance Exception RunException

drvProcessRun :: RunOptions output -> IO (RunResult output)
drvProcessRun ro@RunOptions {..} = do
  let p =
        flip execState (roProc ro) $ do
          modify $ setStdin nullStream
          modify $
            setStderr $
            case roErrors of
              Inherit -> inherit
              Silent  -> nullStream
          for_ roInput $ \input ->
            modify $
            setStdin (byteStringInput $ LBS.fromStrict $ Text.encodeUtf8 input)
  (runExitCode, runOutput) <-
    case roOutput of
      Inherit -> do
        exitCode <- runProcess p
        pure (exitCode, ())
      Silent -> do
        exitCode <- runProcess (p & setStdout nullStream)
        pure (exitCode, ())
      Capture -> do
        (exitCode, out) <- readProcessStdout p
        let output = Text.strip $ Text.decodeUtf8 $ LBS.toStrict out
        pure (exitCode, output)
  let result = RunResult {..}
  when (roCheck && not (runOK result)) $ throw $ RunException roArgs
  pure result
