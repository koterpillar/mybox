{-# LANGUAGE TypeFamilies #-}

module Mybox.Config.Reader where

import Data.ByteString (ByteString)
import Data.Yaml qualified as Yaml
import Effectful.Dispatch.Dynamic

import Mybox.Aeson
import Mybox.Prelude

data Reader :: Effect where
  Host :: Reader m Text
  ReadConfig :: Path -> Reader m ByteString

type instance DispatchOf Reader = Dynamic

readHost :: Reader :> es => Eff es Text
readHost = send Host

readConfigFile :: Reader :> es => Path -> Eff es ByteString
readConfigFile = send . ReadConfig

readConfigYAML :: (FromJSON a, Reader :> es) => Path -> Eff es a
readConfigYAML p =
  readConfigFile p
    >>= Yaml.decodeThrow
    >>= parseThrow (parseJSONWithContext p.text)
