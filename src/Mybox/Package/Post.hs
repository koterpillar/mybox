module Mybox.Package.Post (PackagePost, parsePost, postToJSON, runPost, installWithPost) where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Prelude

type PackagePost p = HasField "post" p [Text]

parsePost :: Object -> Parser [Text]
parsePost o = parseCollapsedList o "post"

postToJSON :: PackagePost p => p -> [Pair]
postToJSON p = ["post" .= p.post]

runPost :: (Driver :> es, PackagePost p) => p -> Eff es ()
runPost p = for_ p.post $ drvRun . shellRaw

installWithPost :: (Driver :> es, PackagePost p) => (p -> Eff es ()) -> p -> Eff es ()
installWithPost f p = f p >> runPost p
