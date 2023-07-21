module Package.LinksSpec where

import           Data.Foldable

import qualified Data.Text        as Text

import           Test.Hspec

import           Package.Links
import           Package.SpecBase

import           Paths_mybox

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
          \psr ->
            fn $
            Links
              { linksSource = undefined
              , linksDestination = undefined
              , linksDot = False
              , linksShallow = False
              , linksOnly = Nothing
              }
      }
