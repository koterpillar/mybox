module Mybox.Package.LinksSpec where

import Mybox.Driver
import Mybox.Filters
import Mybox.Package.Links
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase

data Test = Test
  { modifyPkg :: LinksPackage -> LinksPackage
  , modifyCmd :: Args -> Args
  , baseDir :: forall es. Driver :> es => Eff es (Path Abs)
  , expectedFiles :: [Path Rel]
  , content :: Text
  }

baseLinks :: Test -> PackageSpecArgs -> PackageSpec LinksPackage
baseLinks test psa =
  ps (test.modifyPkg $ mkLinksPackage (mkPath "test/fixtures/links") $ mkPath psa.directory.basename)
    & checkInstalled
      ( do
          base <- baseDir test
          commandHasOutput
            (test.modifyCmd $ "cat" :| [(base </> psa.directory.basename <//> file).text | file <- test.expectedFiles])
            test.content
      )

-- | Like 'baseLinks', but also asserts the destinations are real files
-- (copies) rather than symlinks. The symlink check honours 'modifyCmd' so it
-- runs under sudo for root installs.
copyLinks :: Test -> PackageSpecArgs -> PackageSpec LinksPackage
copyLinks test psa =
  baseLinks test psa
    & checkInstalled
      ( do
          base <- baseDir test
          let dest file = base </> psa.directory.basename <//> file
          commandHasOutput
            (test.modifyCmd $ "cat" :| [(dest file).text | file <- test.expectedFiles])
            test.content
          for_ test.expectedFiles $ \file ->
            modifyDriver test.modifyCmd (drvIsSymlink $ dest file) >>= (`shouldBe` False)
      )

defTest :: Test
defTest =
  Test
    { modifyPkg = id
    , modifyCmd = id
    , baseDir = drvHome
    , expectedFiles = ["myfile", mkPath "deep/space/nine/ncc-1701.txt"]
    , content = "Linked file\nEnterprise"
    }

spec :: Spec
spec = do
  metaSpec
    @LinksPackage
    [ (Nothing, "{\"links\": \"test/test\", \"destination\": \"test\"}")
    , (Just "all fields", "{\"links\": \"test/test\", \"destination\": \"test\", \"dot\": true, \"shallow\": true, \"copy\": true, \"include\": [\"foo\"], \"root\": true}")
    ]
  packageSpecGen "links" $ baseLinks defTest
  packageSpecGen "shallow links" $ baseLinks $ defTest{modifyPkg = \p -> p{shallow = True}}
  packageSpecGen "copy links" $ copyLinks defTest{modifyPkg = \p -> p{copy = True}}
  packageSpecGen "dot links" $
    baseLinks $
      defTest
        { modifyPkg = \p -> p{dot = True}
        , expectedFiles = [".myfile", mkPath ".deep/space/nine/ncc-1701.txt"]
        }
  packageSpecGen "only links" $
    baseLinks $
      defTest
        { modifyPkg = \p -> p{filters = mempty{includes = ["myfile"]}}
        , expectedFiles = ["myfile"]
        , content = "Linked file"
        }
  onlyIf "Root tests pollute real /root and require a virtual system" virtualSystem $ do
    packageSpecGen "root links" $
      baseLinks $
        defTest
          { modifyPkg = \p -> p{root = True}
          , modifyCmd = sudo
          , baseDir = drvHome_ "root"
          }
    packageSpecGen "copy root links" $
      copyLinks $
        defTest
          { modifyPkg = \p -> p{copy = True, root = True}
          , modifyCmd = sudo
          , baseDir = drvHome_ "root"
          }
