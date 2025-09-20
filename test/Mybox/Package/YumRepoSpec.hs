module Mybox.Package.YumRepoSpec where

import Mybox.Driver
import Mybox.Package.SpecBase
import Mybox.Package.YumRepo
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    @YumRepo
    [ (Nothing, "{\"yum_name\": \"test\", \"yum_url\": \"https://example.com/repo\"}")
    , (Just "with GPG key", "{\"yum_name\": \"test\", \"yum_url\": \"https://example.com/repo\", \"gpg_key\": \"https://example.com/key\"}")
    ]
  onlyIfOS "YUM repository tests are only available on Fedora" (\case Linux Fedora -> True; _ -> False) $
    onlyIf "YUM repository tests require virtual system (Docker or CI)" virtualSystem $
      packageSpec $
        ps
          (mkYumRepo "nodesource" "https://rpm.nodesource.com/pub_23.x/nodistro/nodejs/$basearch")
            { gpgKey = Just "https://rpm.nodesource.com/pub/el/NODESOURCE-GPG-SIGNING-KEY-EL"
            }
          & checkInstalledCommandOutput ("yum" :| ["info", "nodejs"]) "23."
