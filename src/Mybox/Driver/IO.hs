module Mybox.Driver.IO (localDriver) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful.Dispatch.Dynamic
import System.Process.ByteString

import Mybox.Driver.Class
import Mybox.Driver.Ops
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

localDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
localDriver =
  interpret_ $ \case
    DrvRun exitBehavior outputBehavior args_ -> do
      let (cmd :| args) = Text.unpack <$> args_
      (exitCode, stdout, stderr) <-
        liftIO $ readProcessWithExitCode cmd args mempty
      exit <-
        case exitBehavior of
          RunExitError ->
            case exitCode of
              ExitSuccess -> pure ()
              ExitFailure code -> error $ errorMessage args_ code stdout stderr
          RunExitReturn -> pure exitCode
      output <-
        case outputBehavior of
          RunOutputShow -> void $ liftIO $ BS.putStr stdout
          RunOutputHide -> pure ()
          RunOutputReturn -> pure $ bsStrip stdout
      pure $ RunResult{..}
