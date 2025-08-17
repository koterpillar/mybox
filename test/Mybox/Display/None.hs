module Mybox.Display.None where

import Effectful.Dispatch.Dynamic

import Mybox.Display.Class
import Mybox.Prelude

noDisplay ::
  forall a es r.
  Monoid (Banner a) =>
  Eff (Display a : es) r -> Eff es r
noDisplay = interpret_ $ \case
  Log !_ -> pure ()
  SetBanner !_ -> pure ()
  GetBanner -> pure mempty
