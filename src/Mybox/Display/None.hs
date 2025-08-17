module Mybox.Display.None where

import Effectful.Dispatch.Dynamic

import Mybox.Display.Class
import Mybox.Prelude

-- | A no-op display handler that discards all display operations
-- This is useful for testing where you don't want any display output
noDisplay ::
  forall a es r.
  Monoid (Banner a) =>
  Eff (Display a : es) r -> Eff es r
noDisplay = interpret_ $ \case
  Log _log -> pure ()
  SetBanner _banner -> pure ()
  GetBanner -> pure mempty
