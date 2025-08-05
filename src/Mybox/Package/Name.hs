module Mybox.Package.Name where

import Data.Text qualified as Text

import Mybox.Prelude

type PackageName a = HasField "name" a Text

pathname :: PackageName p => p -> Text
pathname p = Text.replace "/" "--" p.name
