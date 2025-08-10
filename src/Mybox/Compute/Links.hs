module Mybox.Compute.Links where

import Data.Aeson.Types
import Data.Text qualified as Text
import Network.URI qualified as URI
import Text.HTML.Scalpel

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Driver
import Mybox.Filters
import Mybox.Prelude

parseThrowing :: MonadThrow m => (String -> Maybe a) -> String -> Text -> m a
parseThrowing fn desc value_ =
  let value = Text.unpack value_
   in case fn value of
        Just uri -> pure uri
        Nothing -> throwString $ "Invalid " <> desc <> ": " <> value

uriRelativeTo :: MonadThrow m => Text -> Text -> m Text
uriRelativeTo url_ base_ = do
  url <- parseThrowing URI.parseURIReference "URI reference" url_
  base <- parseThrowing URI.parseURI "URI" base_
  let result = URI.relativeTo url base
  pure $ Text.pack $ URI.uriToString id result ""

linksProcessor :: Driver :> es => Processor (Eff es)
linksProcessor value rest = do
  url <- parseThrow parseJSON value
  args <- parseThrow parseFilter rest
  contents <- drvHttpGet url
  links <-
    maybe (throwString "Cannot find links") pure $
      scrapeStringLike contents $
        attrs "href" "a"
  link <- throwLeft $ choose_ (toFilters args) links
  linkAbsolute <- uriRelativeTo link url
  pure $ Just $ String linkAbsolute
