module Mybox.Package.Links (
  LinksPackage (..),
  LinksMethod (..),
  mkLinksPackage,
) where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Filters
import Mybox.JSONEx
import Mybox.Package.Class
import Mybox.Package.Destination
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Package.Root
import Mybox.Prelude
import Mybox.Tracker
import Mybox.Utils

data LinksPackage = LinksPackage
  { source_ :: Path AnyAnchor
  , destination :: Path AnyAnchor
  , dot :: Bool
  , method :: LinksMethod
  , filters :: FilterFields
  , root :: Bool
  , post :: [Text]
  }
  deriving (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via ViaJSONEx LinksPackage

data LinksMethod
  = LinksMethodLinks
  | LinksMethodShallowLinks
  | LinksMethodCopy
  deriving (Bounded, Enum, Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via ViaJSONEx LinksMethod

instance RecValue LinksMethod where
  rvEmpty = LinksMethodLinks

instance JSONEx LinksMethod where
  jsonEx = enumCodec "LinksMethod" $ \case
    LinksMethodLinks -> "links"
    LinksMethodShallowLinks -> "shallow"
    LinksMethodCopy -> "copy"

mkLinksPackage :: Path AnyAnchor -> Path AnyAnchor -> LinksPackage
mkLinksPackage src dest =
  LinksPackage
    { source_ = src
    , destination = dest
    , dot = False
    , method = LinksMethodLinks
    , filters = mempty
    , root = False
    , post = []
    }

instance PackageName LinksPackage where
  splitName = genericSplitName' @'["links"] @'["source_", "destination", "dot"]

instance JSONEx LinksPackage where
  jsonEx =
    jsonExObject "LinksPackage" $
      field @"source_" "links" "TODO"
        :* field @"destination" "destination" "TODO"
        :* fieldDefault @"dot" "dot" "TODO" False
        :* fieldDefault @"method" "method" "TODO" LinksMethodLinks
        :* fieldFlat @"filters"
        :* fieldRoot
        :* fieldPost
        :* Nil

source :: Driver :> es => LinksPackage -> Eff es (Path Abs)
source p = do
  cwd <- drvCurrentDir
  let src = cwd <//> p.source_
  unlessM (drvIsDir src) $ error $ "Source is not a directory: " <> Text.unpack src.text
  pure src

paths :: Driver :> es => LinksPackage -> Eff es (Set (Path Abs))
paths p = do
  let opt =
        if p.method == LinksMethodShallowLinks
          then findOptions{maxDepth = Just 1}
          else findOptions{onlyFiles = True}
  src <- source p
  pp <- drvFind src opt

  let fs = toFilters p.filters

  pure $ Set.filter (\path_ -> all ($ (pRelativeTo_ src path_).text) fs) pp

-- | A version reflecting what would be installed. Symlinks only need to track
-- the set of source paths (the link target follows the source), but copies are
-- snapshots, so their contents are hashed too to detect when the source
-- changed and the copy needs refreshing.
lpRemoteVersion :: Driver :> es => LinksPackage -> Eff es Text
lpRemoteVersion p = do
  src <- source p
  pp <- Set.toList <$> paths p
  entries <- for pp $ \path_ -> do
    let rel = (pRelativeTo_ src path_).text
    chash <- if p.method == LinksMethodCopy then sha256 <$> drvReadBinaryFile path_ else pure ""
    pure $ rel <> "\0" <> chash
  pure $ sha256 $ Text.encodeUtf8 $ Text.unlines entries

lpInstall :: (Driver :> es, Tracker :> es) => LinksPackage -> Eff es ()
lpInstall p = do
  destination <- destinationPath p
  pp <- paths p
  src <- source p
  sudo' <- mkSudo
  let place =
        if p.method == LinksMethodCopy
          then drvCopy
          else drvLink
  for_ (Set.toList pp) $ \path_ -> do
    let path = pRelativeTo_ src path_
    let pathDot = (if p.dot then mkPath $ "." <> path.text else path)
    let pathDest = destination <//> pathDot
    modifyDriver (if p.root then sudo' else id) $ place path_ pathDest
    trkAdd p pathDest

instance Package LinksPackage where
  localVersion = manualVersion
  remoteVersion = lpRemoteVersion
  install = installWithPost $ manualVersionInstall lpInstall
