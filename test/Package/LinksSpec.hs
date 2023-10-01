module Package.LinksSpec
  ( spec
  ) where

import           Data.Foldable

import qualified Data.Text        as Text

import           Test.Hspec

import           Package.Links
import           Package.SpecBase

mods :: [(String, Links -> Links)]
mods =
  [ ("links: normal", id)
  , ("links: shallow", \l -> l {linksShallow = True})
  , ("links: dot", \l -> l {linksDot = True})
  ]

spec :: Spec
spec =
  for_ mods $ \(name, fn) ->
    packageSpec $
    PackageSpec
      { psName = name
      , psPackage =
          \PackageSpecRun{..} ->
            fn $
            Links
              { linksSource = "tests/package/test_links_content"
              , linksDestination = psrDirectory <> "/links"
              , linksDot = False
              , linksShallow = False
              , linksOnly = Nothing
              }
      }
