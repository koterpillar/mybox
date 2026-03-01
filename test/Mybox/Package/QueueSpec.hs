module Mybox.Package.QueueSpec where

import Data.Set qualified as Set
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Destination
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.Spec.Utils
import Mybox.SpecBase
import Mybox.Tracker

data ActionPackage = ActionPackage
  { name :: Text
  , destination :: Path AnyAnchor
  , delayS :: Text -- Really Int, but genericWithoutName doesn't support it
  , deps :: [ActionPackage]
  }
  deriving (Eq, Generic, Show)

instance HasField "root" ActionPackage Bool where
  getField = const False

instance PackageName ActionPackage where
  withoutName = genericWithoutName

instance FromJSON ActionPackage where
  parseJSON = withObjectTotal "ActionPackage" $ do
    name <- takeField "name"
    destination <- takeField "destination"
    delayS <- takeField "delayS"
    deps <- takeField "deps"
    pure ActionPackage{..}

instance ToJSON ActionPackage where
  toJSON p =
    object
      [ "name" .= p.name
      , "destination" .= p.destination
      , "delayS" .= p.delayS
      , "deps" .= p.deps
      ]

instance Package ActionPackage where
  remoteVersion _ = pure "1"
  localVersion _ = pure Nothing
  install p = do
    queueInstallMany p.deps
    d <- destinationPath p
    drvRun $ "sleep" :| [p.delayS]
    drvRun $ shellRaw $ "echo " <> p.name <> " >> " <> d.text

spec :: Spec
spec = do
  describe "ActionPackage" $ metaSpec @ActionPackage [(Nothing, "{\"name\":\"test\",\"destination\":\"test\",\"delayS\":\"1\",\"deps\":[]}")]
  describe "runInstallQueue" $ do
    let pkgs :: Path AnyAnchor -> [ActionPackage]
        pkgs fileName =
          [ one
          , mkPkg "four" 0 [mkPkg "four.dep" 4 []]
          , mkPkg "one.rdep" 0 [one]
          , mkPkg "two" 2 []
          , mkPkg "three" 3 []
          ]
         where
          mkPkg name delay deps = ActionPackage{destination = fileName, ..}
           where
            delayS = Text.pack $ show (delay :: Int)
          one = mkPkg "one" 1 []

    let actualOrder :: Driver :> es => Path AnyAnchor -> Eff es [Text]
        actualOrder path = do
          home <- drvHome
          logs <- drvReadFile (home <//> path)
          pure $ Text.lines logs

    it "installs packages with dependencies" $ do
      fileName <- mkPath <$> randomText "queue-spec-test"

      nullTracker $ runInstallQueue $ queueInstallMany $ pkgs fileName

      order <- actualOrder fileName

      Set.fromList order `shouldBe` Set.fromList ["one", "one.rdep", "two", "three", "four", "four.dep"]

      let filterAny names = filter (`elem` names)
      -- "one.rdep" depends on already-installed "one"
      filterAny ["one", "one.rdep"] order `shouldBe` ["one", "one.rdep"]
      -- "four" would have gone first, but waited for "four.dep"
      filterAny ["four", "four.dep"] order `shouldBe` ["four.dep", "four"]
