{-# LANGUAGE GADTs #-}

module Driver
  ( Driver
  , drvRoot
  , drvWithRoot
  , drvRun
  , drvRunOK
  , drvRunOutput
  , OutStream(..)
  , RunOptions(..)
  , RunResult(..)
  , drvRunOptions
  , RunException(..)
  , drvLocal
  , drvModify
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

roProc :: RunOptions output -> ProcessConfig () () ()
roProc RunOptions {..} =
  let executable :| args = roArgs
   in proc (Text.unpack executable) (map Text.unpack args)

type DriverRun = forall output. Bool -> RunOptions output -> IO (RunResult output)

data Driver =
  Driver
    { drvRun_ :: DriverRun
    , drvRoot :: Bool
    }

drvModify :: (DriverRun -> DriverRun) -> Driver -> Driver
drvModify f driver = driver {drvRun_ = f (drvRun_ driver)}

drvWithRoot :: Bool -> Driver -> Driver
drvWithRoot root driver = driver {drvRoot = root}

drvRunOptions :: Driver -> RunOptions output -> IO (RunResult output)
drvRunOptions Driver {..} = drvRun_ drvRoot

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

drvLocal :: Driver
drvLocal = Driver {drvRun_ = drvLocalRun, drvRoot = False}

drvLocalRun :: Bool -> RunOptions output -> IO (RunResult output)
drvLocalRun False ro = drvProcessRun ro
drvLocalRun True ro = do
  _ <- drvProcessRun $ (roDefaults $ "sudo" :| ["-v"]) {roOutput = Inherit}
  drvProcessRun $ roPrependArgs ["sudo"] ro

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
