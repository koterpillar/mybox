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

import           System.Process.Typed

data RunResult =
  RunResult
    { runOK     :: Bool
    , runOutput :: Maybe Text
    }
  deriving (Eq, Show)

data RunOptions =
  RunOptions
    { roArgs          :: NonEmpty Text
    , roCheck         :: Bool
    , roCaptureOutput :: Bool
    , roSilent        :: Bool
    , roInput         :: Maybe Text
    }

roDefaults :: NonEmpty Text -> RunOptions
roDefaults args =
  RunOptions
    { roArgs = args
    , roCheck = True
    , roCaptureOutput = False
    , roSilent = False
    , roInput = Nothing
    }

roPrependArgs :: [Text] -> RunOptions -> RunOptions
roPrependArgs args ro = ro {roArgs = NonEmpty.prependList args (roArgs ro)}

roProc :: RunOptions -> ProcessConfig () () ()
roProc RunOptions {..} =
  let executable :| args = roArgs
   in proc (Text.unpack executable) (map Text.unpack args)

data Driver =
  Driver
    { drvRun_ :: Bool -> RunOptions -> IO RunResult
    , drvRoot :: Bool
    }

drvWithRoot :: Bool -> Driver -> Driver
drvWithRoot root driver = driver {drvRoot = root}

drvRunO :: Driver -> RunOptions -> IO RunResult
drvRunO Driver {..} = drvRun_ drvRoot

drvRun :: NonEmpty Text -> Driver -> IO ()
drvRun args driver = void $ drvRunO driver $ roDefaults args

drvRunOK :: NonEmpty Text -> Driver -> IO Bool
drvRunOK args driver = runOK <$> drvRunO driver ro
  where
    ro = (roDefaults args) {roCheck = False}

drvRunOutput :: NonEmpty Text -> Driver -> IO Text
drvRunOutput args driver = do
  result <- drvRunO driver ro
  case runOutput result of
    Nothing     -> error "drvRunOutput: no output"
    Just output -> return output
  where
    ro = (roDefaults args) {roCaptureOutput = True, roSilent = True}

drvLocal :: Driver
drvLocal = Driver {drvRun_ = drvLocalRun, drvRoot = False}

drvLocalRun :: Bool -> RunOptions -> IO RunResult
drvLocalRun False = drvProcessRun
drvLocalRun True  = drvProcessRun . roPrependArgs ["sudo"]

newtype RunException =
  RunException (NonEmpty Text)
  deriving (Eq, Show)

instance Exception RunException

drvProcessRun :: RunOptions -> IO RunResult
drvProcessRun ro@RunOptions {..} = do
  let p =
        flip execState (roProc ro) $ do
          modify $ setStdin nullStream
          when roSilent $ modify $ setStderr nullStream
          for_ roInput $ \input ->
            modify $
            setStdin (byteStringInput $ LBS.fromStrict $ Text.encodeUtf8 input)
  result <-
    if roCaptureOutput
      then do
        (exitCode, out, _) <- readProcess p
        let output = Text.strip $ Text.decodeUtf8 $ LBS.toStrict out
        pure $
          RunResult {runOK = exitCode == ExitSuccess, runOutput = Just output}
      else do
        exitCode <- runProcess (p & setStdout nullStream)
        pure $ RunResult {runOK = exitCode == ExitSuccess, runOutput = Nothing}
  when (roCheck && not (runOK result)) $ throw $ RunException roArgs
  pure result
