module Mybox.Compute.JSONPath where

import Data.Aeson.JSONPath
import Data.Bifunctor (first)
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Compute.Base
import Mybox.Filters
import Mybox.Prelude

jsonpathProcessor :: Processor (Eff es)
jsonpathProcessor pathValue rest = do
  jsonpath_ <- parseThrow parseJSON pathValue
  let jsonpath = if Text.isPrefixOf "$" jsonpath_ then jsonpath_ else "$." <> jsonpath_
  baseString <- parseThrow (.: "base") rest
  base <- jsonDecode "base" baseString
  args <- parseThrow parseFilter rest
  resultValues <- throwLeft $ first show $ query (Text.unpack jsonpath) base
  results <- traverse (parseThrow parseJSON) $ toList resultValues
  result <- throwLeft $ choose_ (filters args) results
  pure $ String result
