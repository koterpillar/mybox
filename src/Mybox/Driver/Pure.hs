module Mybox.Driver.Pure where

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful.Dispatch.Dynamic

import Mybox.Driver.Class
import Mybox.Prelude

pureDriver :: (Args -> Maybe Text) -> Eff (Driver : es) a -> Eff es a
pureDriver run = interpret_ $ \case
  DrvRun exitBehavior outputBehavior args ->
    case run args of
      Just result -> do
        let exit = case exitBehavior of
              RunExitError -> ()
              RunExitReturn -> ExitSuccess
        let output = case outputBehavior of
              RunOutputShow -> ()
              RunOutputHide -> ()
              RunOutputReturn -> Text.encodeUtf8 result
        pure $ RunResult{..}
      Nothing -> do
        let exit = case exitBehavior of
              RunExitError -> terror $ "Unexpected command: " <> Text.pack (show args)
              RunExitReturn -> ExitFailure 1
        let output = case outputBehavior of
              RunOutputShow -> ()
              RunOutputHide -> ()
              RunOutputReturn -> ""
        pure $ RunResult{..}
