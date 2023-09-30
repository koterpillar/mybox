{-# LANGUAGE GADTs #-}

module Driver.Process
  ( drvProcess
  ) where

import           Control.Exception.Base

import           Control.Monad          (when)

import           Control.Monad.State    (execState, modify)

import           Data.Foldable          (for_)

import           System.Process.Typed   hiding (Inherit)

import Driver.Types
import Process

roProc :: RunOptions output -> ProcessConfig () () ()
roProc RunOptions {..} = procText roArgs

drvProcess :: Driver
drvProcess =
  Driver $ \ro@RunOptions {..} -> do
    let p =
          flip execState (roProc ro) $ do
            modify $ setStdin nullStream
            modify $
              setStderr $
              case roErrors of
                Inherit -> inherit
                Silent  -> nullStream
            for_ roInput $ \input ->
              modify $ setStdin (byteStringInput $ procEncode input)
    (runExitCode, runOutput) <-
      case roOutput of
        Inherit -> do
          exitCode <- runProcess p
          pure (exitCode, ())
        Silent -> do
          exitCode <- runProcess (setStdout nullStream p)
          pure (exitCode, ())
        Capture -> do
          (exitCode, out) <- readProcessStdout p
          pure (exitCode, procDecode out)
    let result = RunResult {..}
    when (roCheck && not (runOK result)) $ throw $ RunException roArgs
    pure result
