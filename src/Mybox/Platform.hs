module Mybox.Platform where

import Mybox.Prelude

data Architecture = X86_64 | Aarch64 deriving (Eq, Ord, Show)

data Distribution = Debian {variant :: Text} | Fedora deriving (Eq, Ord, Show)

data OS = Linux {distribution :: Distribution} | MacOS deriving (Eq, Ord, Show)
