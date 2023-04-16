module Package.CloneSpec
  ( spec
  ) where

import qualified Data.Text        as Text

import           Test.Hspec

import           Package.Clone
import           Package.SpecBase

spec :: Spec
spec =
  packageSpec $
  PackageSpec
    { psName = "clone: ohmyzsh"
    , psPackage =
        \psr ->
          Clone
            {cloneRepo = "ohmyzsh/ohmyzsh", cloneDestination = psrDirectory psr}
    , psCheckInstalled =
        \psr ->
          psCheckInstalledOutput psr
            ("cat" :|
             [Text.pack (psrDirectory psr) <> "/templates/zshrc.zsh-template"])
            "alias ohmyzsh"
    }
