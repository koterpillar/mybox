module Mybox.Package.Links (
  LinksPackage (..),
  mkLinksPackage,
) where

import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Destination
import Mybox.Package.ManualVersion
import Mybox.Package.Post
import Mybox.Prelude
import Mybox.Tracker

data LinksPackage = LinksPackage
  { source_ :: Path AnyAnchor
  , destination :: Path AnyAnchor
  , dot :: Bool
  , shallow :: Bool
  , only :: Maybe [Path AnyAnchor]
  , post :: [Text]
  }
  deriving (Eq, Show)

mkLinksPackage :: Path AnyAnchor -> Path AnyAnchor -> LinksPackage
mkLinksPackage src dest =
  LinksPackage
    { source_ = src
    , destination = dest
    , dot = False
    , shallow = False
    , only = Nothing
    , post = []
    }

instance HasField "name" LinksPackage Text where
  getField p = Text.intercalate "-" ["links", p.source_.text, p.destination.text, Text.pack (show p.dot)]

instance FromJSON LinksPackage where
  parseJSON = withObjectTotal "LinksPackage" $ do
    source_ <- takeField "links"
    destination <- takeField "destination"
    dot <- fromMaybe False <$> takeFieldMaybe "dot"
    shallow <- fromMaybe False <$> takeFieldMaybe "shallow"
    only <- takeCollapsedListMaybe "only"
    post <- takePost
    pure LinksPackage{..}

instance ToJSON LinksPackage where
  toJSON p =
    object $
      [ "links" .= p.source_
      , "destination" .= p.destination
      , "dot" .= p.dot
      , "shallow" .= p.shallow
      , "only" .= p.only
      ]
        <> postToJSON p

source :: Driver :> es => LinksPackage -> Eff es (Path Abs)
source p = do
  cwd <- drvCurrentDir
  let src = cwd <//> p.source_
  unlessM (drvIsDir src) $ error $ "Source is not a directory: " <> Text.unpack src.text
  pure src

paths :: Driver :> es => LinksPackage -> Eff es (Set (Path Abs))
paths p = do
  let opt = if p.shallow then mempty{maxDepth = Just 1} else mempty{onlyFiles = True}
  pp <- source p >>= (`drvFind` opt)

  cwd <- drvCurrentDir
  let only_ = map (cwd <//>) <$> p.only

  case only_ of
    Nothing -> pure pp
    Just only -> pure $ Set.filter (`elem` only) pp

lpRemoteVersion :: Driver :> es => LinksPackage -> Eff es Text
lpRemoteVersion p = Text.intercalate "#" . map (.text) . Set.toList <$> paths p -- FIXME: hash

lpInstall :: (Driver :> es, Tracker :> es) => LinksPackage -> Eff es ()
lpInstall p = do
  destination <- destinationPath p
  pp <- paths p
  src <- source p
  for_ (Set.toList pp) $ \path_ -> do
    let path = pRelativeTo_ src path_
    let pathDot = (if p.dot then mkPath $ "." <> path.text else path)
    let pathDest = destination <//> pathDot
    drvLink path_ pathDest
    trkAdd p pathDest

instance Package LinksPackage where
  localVersion = manualVersion
  remoteVersion = lpRemoteVersion
  install = installWithPost $ manualVersionInstall lpInstall
