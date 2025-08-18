module Mybox.Display.Class (
  Display (..),
  Log,
  Banner,
  displayLog,
  displaySetBanner,
  displayModifyBanner,
  displayModifyBannerWhile,
  DisplayImpl (..),
  runDisplayImpl,
) where

import Data.Kind
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Prelude hiding (log)

import Mybox.Prelude

data family Log a

data family Banner a

data Display :: Type -> Effect where
  Log :: Log a -> Display a m ()
  SetBanner :: Banner a -> Display a m ()
  GetBanner :: Display a m (Banner a)

type instance DispatchOf (Display _) = Dynamic

displayLog :: Display a :> es => Log a -> Eff es ()
displayLog log = send $ Log log

displaySetBanner :: Display a :> es => Banner a -> Eff es ()
displaySetBanner banner = send $ SetBanner banner

displayGetBanner :: Display a :> es => Eff es (Banner a)
displayGetBanner = send GetBanner

displayModifyBanner :: Display a :> es => (Banner a -> Banner a) -> Eff es ()
displayModifyBanner f = displayGetBanner >>= displaySetBanner . f

displayModifyBannerWhile :: Display a :> es => (Banner a -> Banner a) -> Eff es r -> Eff es r
displayModifyBannerWhile f act = do
  banner <- displayGetBanner
  displaySetBanner (f banner)
  act `finally` displaySetBanner banner

data DisplayImpl :: Type -> Effect where
  DrawLog :: Log a -> DisplayImpl a m ()
  DrawBanner :: Banner a -> DisplayImpl a m ()

type instance DispatchOf (DisplayImpl _) = Dynamic

runDisplayImpl ::
  forall a es r.
  (DisplayImpl a :> es, Monoid (Banner a)) =>
  Eff (Display a : es) r ->
  Eff es r
runDisplayImpl =
  reinterpret_
    (evalState @(Banner a) mempty)
    ( \case
        Log log -> send $ DrawLog log
        SetBanner banner -> do
          put banner
          send $ DrawBanner banner
        GetBanner -> get
    )
