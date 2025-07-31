module Mybox.SpecHook where

import Test.Hspec qualified as Hspec

import Mybox.SpecBase

hook :: Spec -> Hspec.Spec
hook = effSpec
