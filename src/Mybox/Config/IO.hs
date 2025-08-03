module Mybox.Config.IO where

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Effectful.Dispatch.Dynamic

import Mybox.Config.Reader
import Mybox.Driver
import Mybox.Prelude

runReaderIO :: (Driver :> es, IOE :> es) => Eff (Reader : es) a -> Eff es a
runReaderIO = interpret_ $ \case
  ReadConfig path -> liftIO $ BS.readFile $ Text.unpack path.text
  Host -> drvRunOutput $ "hostname" :| []
