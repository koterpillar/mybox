module Mybox.Package.LinksSpec where

import Mybox.Package.Links
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

baseLinks :: (LinksPackage -> LinksPackage) -> [Path Rel] -> PackageSpecArgs -> PackageSpec LinksPackage
baseLinks modifyPkg expectedFiles psa =
  ps (modifyPkg $ mkLinksPackage (mkPath "test/fixtures/links") $ pWiden psa.directory)
    & checkInstalledCommandOutput
      ("cat" :| [(psa.directory <//> file).text | file <- expectedFiles])
      "Linked file\nEnterprise"

spec :: Spec
spec = do
  jsonSpec
    (Nothing @LinksPackage)
    [ (Nothing, "{\"links\": \"test/test\", \"destination\": \"test\"}")
    , (Just "all fields", "{\"links\": \"test/test\", \"destination\": \"test\", \"dot\": true, \"shallow\": true, \"only\": [\"only1\", \"only2\"]}")
    ]
  let expectedFilesNoDot = ["myfile", mkPath "deep/space/nine/ncc-1701.txt"]
  packageSpec $ baseLinks id expectedFilesNoDot
  packageSpec $ baseLinks (\p -> p{shallow = True}) expectedFilesNoDot
  packageSpec $ baseLinks (\p -> p{dot = True}) [".myfile", mkPath ".deep/space/nine/ncc-1701.txt"]
