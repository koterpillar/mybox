module Package.Links
  ( Links(..)
  ) where

import           Crypto.Hash.SHA256 as SHA256

import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

import           System.Directory

import           Driver

import           Package
import           Package.Manual

data Links =
  Links
    { linksSource      :: FilePath
    , linksDestination :: FilePath
    , linksDot         :: Bool
    , linksShallow     :: Bool
    , linksOnly        :: Maybe [Text]
    }
  deriving (Eq, Show)

linksPaths :: Links -> IO [FilePath]
linksPaths pk = filter (linksFilter pk) <$> do
  if linksShallow pk
    then getDirectoryContents $ linksSource pk
    else error "linksPaths: non-shallow not implemented"

linksFilter :: Links -> FilePath -> Bool
linksFilter pk =
  case linksOnly pk of
    Nothing   -> const True
    Just only -> (`elem` only) . Text.pack

instance ManualPackage Links where
  mpkInstall = error "mpkInstall for Links: not implemented"
  mpkRemoteVersion _ pk = do
    paths <- linksPaths pk
    let ctx0 = SHA256.init
    let ctx = foldl SHA256.update ctx0 (map (Text.encodeUtf8 . Text.pack) paths)
    let digest = SHA256.finalize ctx
    pure $ Text.decodeUtf8 digest

deriving via (MPK Links) instance Package Links
