module Environment where

import           Data.Text          (Text)
import qualified Data.Text          as Text

import qualified System.Environment as SE

data OS
  = Linux
  | Macos
  deriving (Eq, Ord, Show, Enum, Bounded)

currentOS :: IO OS
currentOS = pure Linux -- FIXME

getEnv :: Text -> IO Text
getEnv = fmap Text.pack . SE.getEnv . Text.unpack

home :: IO Text
home = getEnv "HOME"

localBin :: IO Text
localBin = (\h -> h </> ".local" </> "bin") <$> home

(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b
