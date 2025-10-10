module Mybox.Display.Simple (runSimpleDisplay) where

import Data.List (delete)
import Data.Text qualified as Text
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared
import Prelude hiding (log)

import Mybox.Display.Class
import Mybox.Display.Print (Print)
import Mybox.Display.Print qualified as Print
import Mybox.Prelude

runSimpleDisplay ::
  forall a es r.
  ( ANSIDisplayable a
  , Print :> es
  ) =>
  Eff (Display a : es) r ->
  Eff es r
runSimpleDisplay =
  reinterpret_
    (evalState @[Banner a] mempty)
    $ \case
      Log log -> Print.print $ dumbShow log
      AddBanner banner -> do
        modify $ (banner :)
        get >>= dumbPrint @(Banner a)
      RemoveBanner banner -> do
        modify $ delete banner
        get >>= dumbPrint @(Banner a)

dumbShow :: TerminalShow a => a -> String
dumbShow = Text.unpack . Text.unlines . map (Text.concat . map (.text)) . terminalShow Nothing

dumbPrint :: (Monoid a, Print :> es, TerminalShow a) => [a] -> Eff es ()
dumbPrint = Print.print . dumbShow . mconcat
