module Driver
  ( Driver
  , drvRoot
  , drvWithRoot
  , drvRunOK
  , drvRunOutput
  , drvLocal
  , NonEmpty(..)
  ) where

import qualified Data.ByteString.Lazy as LBS

import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NonEmpty

import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text

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
    }

roDefaults :: NonEmpty Text -> RunOptions
roDefaults args =
  RunOptions
    {roArgs = args, roCheck = True, roCaptureOutput = False, roSilent = False}

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

drvRun :: Driver -> RunOptions -> IO RunResult
drvRun Driver {..} = drvRun_ drvRoot

drvWithRoot :: Bool -> Driver -> Driver
drvWithRoot root driver = driver {drvRoot = root}

drvRunOK :: NonEmpty Text -> Driver -> IO Bool
drvRunOK args driver = runOK <$> drvRun driver ro
  where
    ro = roDefaults args

drvRunOutput :: NonEmpty Text -> Driver -> IO Text
drvRunOutput args driver = do
  result <- drvRun driver ro
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

drvProcessRun :: RunOptions -> IO RunResult
drvProcessRun ro@RunOptions {..} =
  if roCaptureOutput
    then do
      (exitCode, out, _) <- readProcess $ roProc ro
      let output = Text.strip $ Text.decodeUtf8 $ LBS.toStrict out
      pure $
        RunResult {runOK = exitCode == ExitSuccess, runOutput = Just output}
    else do
      exitCode <- runProcess $ roProc ro
      pure $ RunResult {runOK = exitCode == ExitSuccess, runOutput = Nothing}
