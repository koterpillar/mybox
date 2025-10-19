module Mybox.Driver.IO (localDriver) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful.Dispatch.Dynamic
import System.Process.Typed

import Mybox.Driver.Class
import Mybox.Driver.Ops
import Mybox.LockMap
import Mybox.Prelude

errorMessage :: Args -> Int -> ByteString -> ByteString -> String
errorMessage args code stdout stderr =
  baseMessage
    <> extraMessage stderr "stderr"
    <> extraMessage stdout_ "stdout"
 where
  stdout_ = fromMaybe stdout $ BS.stripPrefix "OCI runtime exec failed: exec failed: " stdout
  baseMessage =
    "Process "
      <> Text.unpack (shellJoin args)
      <> " failed with exit code "
      <> show code
  extraMessage str desc
    | BS.null str = ""
    | otherwise = "; " <> desc <> ": " <> Text.unpack (Text.decodeUtf8 str)

bsStrip :: ByteString -> ByteString
bsStrip = BS.dropWhileEnd whitespace . BS.dropWhile whitespace
 where
  whitespace 0x20 = True -- space
  whitespace 0x09 = True -- tab
  whitespace 0x0A = True -- newline
  whitespace 0x0D = True -- carriage return
  whitespace _ = False

localDriver :: (Concurrent :> es, IOE :> es) => Eff (Driver : es) a -> Eff es a
localDriver act = do
  lm <- newLockMap @_ @[Text] @()
  interpretWith_ act $ \case
    DrvRun exitBehavior outputBehavior args_ -> do
      let (cmd :| args) = Text.unpack <$> args_
      let process = proc cmd args
      (exitCode :: ExitCode, stdout_, stderr_) <-
        case outputBehavior of
          RunOutputShow -> do
            (exitCode, stderr) <- liftIO $ readProcessStderr $ process & setStdout inherit
            pure (exitCode, mempty, stderr)
          RunOutputHide -> do
            (exitCode, stderr) <- liftIO $ readProcessStderr $ process & setStdout nullStream
            pure (exitCode, mempty, stderr)
          RunOutputReturn -> liftIO $ readProcess process
      let stdout = bsStrip $ LBS.toStrict stdout_
      let stderr = bsStrip $ LBS.toStrict stderr_
      exit <-
        case exitBehavior of
          RunExitError ->
            case exitCode of
              ExitSuccess -> pure ()
              ExitFailure code -> error $ errorMessage args_ code stdout stderr
          RunExitReturn -> pure exitCode
      let output =
            case outputBehavior of
              RunOutputShow -> ()
              RunOutputHide -> ()
              RunOutputReturn -> stdout
      pure $ RunResult{..}
    DrvLock key -> lockMapGet lm key ()
