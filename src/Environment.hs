module Environment where

data OS
  = Linux
  | Macos
  deriving (Eq, Ord, Show, Enum, Bounded)

currentOS :: IO OS
currentOS = pure Linux -- FIXME
