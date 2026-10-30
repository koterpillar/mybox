module Mybox.Package.Post (PackagePost, fieldPost, runPost, installWithPost) where

import Mybox.Driver
import Mybox.JSONEx
import Mybox.Prelude

type PackagePost p = HasField "post" p [Text]

fieldPost :: Field "post" [Text]
fieldPost = fieldDefault "post" "TODO" []

runPost :: (Driver :> es, PackagePost p) => p -> Eff es ()
runPost p = for_ p.post $ drvRun . shellRaw

installWithPost :: (Driver :> es, PackagePost p) => (p -> Eff es ()) -> p -> Eff es ()
installWithPost f p = f p >> runPost p
