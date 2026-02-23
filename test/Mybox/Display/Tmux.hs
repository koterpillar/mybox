module Mybox.Display.Tmux (runTmux, colourString) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async
import System.IO

import Mybox.Display.Print (Print)
import Mybox.Display.Print qualified as Print
import Mybox.Driver
import Mybox.Prelude
import Mybox.Spec.Utils

whileMDelay :: Concurrent :> es => Eff es Bool -> Eff es ()
whileMDelay act = whileM $ do
  r <- act
  unless r $ threadDelay 100000
  pure r

runTmux :: (Concurrent :> es, IOE :> es) => Eff (Print : es) r -> Eff es (r, ByteString)
runTmux act = localDriver $ do
  tmuxBinary <- drvRunOutput $ "which" :| ["tmux"]
  socketName <- randomText "tmux"
  let tmux :: Args -> Args
      tmux args = tmuxBinary :| ["-L", socketName] ++ toList args
  let tmuxRunning = do
        isRunning <- drvRunOutputExit $ tmux $ "list-sessions" :| []
        pure $ isRunning.exit == ExitSuccess
  drvTempFile $ \out -> do
    drvTempFile $ \script -> do
      r <- drvTempFile $ \lock -> do
        drvWriteFile script $
          Text.unlines
            [ "#!/bin/sh"
            , "while test -f " <> lock.text <> "; do sleep 0.1; done"
            , shellJoin $ tmux $ "capture-pane" :| ["-e"]
            , shellJoin $ tmux $ "save-buffer" :| [out.text]
            , shellJoin $ tmux $ "kill-server" :| []
            ]
        drvMakeExecutable script
        _ <- async $ drvRunSilent $ tmux $ "-C" :| ["new-session", script.text]
        whileMDelay $ not <$> tmuxRunning
        tty <- drvRunOutput $ tmux $ "display-message" :| ["-p", "#{pane_tty}"]
        bracket
          (liftIO $ openFile (Text.unpack tty) ReadWriteMode)
          (liftIO . hClose)
          $ \hTty -> Print.run hTty $ inject act
      whileMDelay tmuxRunning
      output <- drvRunOutputBinary $ "cat" :| [out.text]
      pure (r, BS.dropWhileEnd (== 10) output)

colourString :: Text -> ByteString
colourString =
  Text.encodeUtf8
    . Text.replace "<red>" "\ESC[31m"
    . Text.replace "<green>" "\ESC[32m"
    . Text.replace "<yellow>" "\ESC[33m"
    . Text.replace "<blue>" "\ESC[34m"
    . Text.replace "<magenta>" "\ESC[35m"
    . Text.replace "<reset>" "\ESC[39m"
