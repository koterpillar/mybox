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
  AddBanner !_ -> pure ()
  RemoveBanner !_ -> pure ()
