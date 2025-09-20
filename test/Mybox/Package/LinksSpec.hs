module Mybox.Package.LinksSpec where

import Mybox.Package.Links
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

data Test = Test
  { modifyPkg :: LinksPackage -> LinksPackage
  , expectedFiles :: [Path Rel]
  , content :: Text
  }

baseLinks :: Test -> PackageSpecArgs -> PackageSpec LinksPackage
baseLinks test psa =
  ps (test.modifyPkg $ mkLinksPackage (mkPath "test/fixtures/links") $ pWiden psa.directory)
    & checkInstalledCommandOutput
      ("cat" :| [(psa.directory <//> file).text | file <- test.expectedFiles])
      test.content

defTest :: Test
defTest =
  Test
    { modifyPkg = id
    , expectedFiles = ["myfile", mkPath "deep/space/nine/ncc-1701.txt"]
    , content = "Linked file\nEnterprise"
    }

spec :: Spec
spec = do
  jsonSpec
    @LinksPackage
    [ (Nothing, "{\"links\": \"test/test\", \"destination\": \"test\"}")
    , (Just "all fields", "{\"links\": \"test/test\", \"destination\": \"test\", \"dot\": true, \"shallow\": true, \"only\": [\"only1\", \"only2\"]}")
    ]
  packageSpecGen "links" $ baseLinks defTest
  packageSpecGen "shallow links" $ baseLinks $ defTest{modifyPkg = \p -> p{shallow = True}}
  packageSpecGen "dot links" $
    baseLinks $
      defTest
        { modifyPkg = \p -> p{dot = True}
        , expectedFiles = [".myfile", mkPath ".deep/space/nine/ncc-1701.txt"]
        }
  packageSpecGen "only links" $
    baseLinks $
      defTest
        { modifyPkg = \p -> p{only = Just [mkPath "myfile"]}
        , expectedFiles = ["myfile"]
        , content = "Linked file"
        }
