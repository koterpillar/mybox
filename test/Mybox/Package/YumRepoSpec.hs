module Mybox.Package.YumRepoSpec where

import Mybox.Driver
import Mybox.Package.SpecBase
import Mybox.Package.YumRepo
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    (Nothing @YumRepo)
    [ (Nothing, "{\"yum_name\": \"test\", \"yum_url\": \"https://example.com/repo\"}")
    , (Just "with GPG key", "{\"yum_name\": \"test\", \"yum_url\": \"https://example.com/repo\", \"gpg_key\": \"https://example.com/key\"}")
    ]
  onlyIfOS (\case Linux Fedora -> True; _ -> False) $
    onlyIf virtualSystem $
      packageSpec $ \_ ->
        ps
          (mkYumRepo "nodesource" "https://rpm.nodesource.com/pub_23.x/nodistro/nodejs/$basearch")
            { gpgKey = Just "https://rpm.nodesource.com/pub/el/NODESOURCE-GPG-SIGNING-KEY-EL"
            }
          & checkInstalledCommandOutput ("yum" :| ["info", "nodejs"]) "23."
