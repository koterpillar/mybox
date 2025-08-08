module Mybox.Package.FlatpakSpec where

import Mybox.Driver
import Mybox.Package.Flatpak
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    (Nothing @FlatpakPackage)
    [ (Nothing, "{\"flatpak\": \"com.example.Test\"}")
    ]
  onlyIf inCI $
    skipIf inDocker $
      onlyIfOS (\case Linux _ -> True; _ -> False) $
        packageSpec $ \_ ->
          ps (mkFlatpakPackage "org.videolan.VLC")
            & checkInstalledCommandOutput
              ("flatpak" :| ["run", "org.videolan.VLC", "--version"])
              "VLC version"
            & ignorePath (mkPath ".local/share/flatpak")
