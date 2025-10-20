module Mybox.Package.FlatpakSpec where

import Mybox.Driver
import Mybox.Package.Flatpak
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  jsonSpec
    @FlatpakPackage
    [ (Nothing, "{\"flatpak\": \"com.example.Test\"}")
    ]
  onlyIf "Flatpak package tests require CI environment" inCI $
    skipIf "Flatpak package tests cannot run in Docker" inDocker $
      onlyIfOS "Flatpak package tests are only available on Linux" (\case Linux _ -> True; _ -> False) $
        packageSpec $
          ps (mkFlatpakPackage "org.videolan.VLC")
            & checkInstalledCommandOutput
              ("flatpak" :| ["run", "org.videolan.VLC", "--version"])
              "VLC version"
            & ignorePaths [mkPath ".local/share/flatpak"]
