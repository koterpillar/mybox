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
    @LinksPackage
    [ (Nothing, "{\"links\": \"test/test\", \"destination\": \"test\"}")
    , (Just "all fields", "{\"links\": \"test/test\", \"destination\": \"test\", \"dot\": true, \"shallow\": true, \"only\": [\"only1\", \"only2\"]}")
    ]
  let expectedFilesNoDot = ["myfile", mkPath "deep/space/nine/ncc-1701.txt"]
  packageSpecGen "links" $ baseLinks id expectedFilesNoDot
  packageSpecGen "shallow links" $ baseLinks (\p -> p{shallow = True}) expectedFilesNoDot
  packageSpecGen "dot links" $ baseLinks (\p -> p{dot = True}) [".myfile", mkPath ".deep/space/nine/ncc-1701.txt"]
