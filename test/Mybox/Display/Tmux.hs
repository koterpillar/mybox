module Mybox.Display.Tmux (runTmux, colourString) where

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Mybox.Display.Print (Print)
import Mybox.Display.Print qualified as Print
import Mybox.Driver
import Mybox.Prelude
import Mybox.Spec.Utils

runTmux ::
  IOE :> es =>
  Eff (Print : es) r ->
  Eff es (r, ByteString)
runTmux act =
  localDriver $ do
    (r, contents) <- Print.runPure $ inject act
    o <- catViaTmux contents
    pure (r, BS.dropWhileEnd (== 10) o)

catViaTmux :: (Driver :> es, IOE :> es) => String -> Eff es ByteString
catViaTmux contents = do
  tmux <- drvRunOutput $ "which" :| ["tmux"]
  socketName <- randomText "tmux"
  let sendTmux arg = tmux <> " -L " <> socketName <> " " <> arg
  drvTempFile $ \input -> do
    drvWriteFile input $ Text.pack contents
    drvTempFile $ \out -> do
      drvTempFile $ \script -> do
        drvWriteFile script $
          Text.unlines
            [ "#!/bin/sh"
            , "cat " <> input.text
            , sendTmux "capture-pane -e"
            , sendTmux $ "save-buffer " <> out.text
            , sendTmux "kill-server"
            ]
        drvMakeExecutable script
        drvRunSilent $ tmux :| ["-C", "-L", socketName, "new-session", script.text]
        whileM $ do
          tmuxRunning <- drvRunOutputExit $ tmux :| ["-L", socketName, "list-sessions"]
          case tmuxRunning.exit of
            ExitSuccess -> pure True
            _ -> do
              liftIO $ threadDelay 100000
              pure False
      drvRunOutputBinary $ "cat" :| [out.text]

colourString :: Text -> ByteString
colourString =
  Text.encodeUtf8
    . Text.replace "<red>" "\ESC[31m"
    . Text.replace "<green>" "\ESC[32m"
    . Text.replace "<yellow>" "\ESC[33m"
    . Text.replace "<blue>" "\ESC[34m"
    . Text.replace "<reset>" "\ESC[39m"
