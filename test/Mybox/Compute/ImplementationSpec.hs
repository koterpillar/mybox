module Mybox.Compute.ImplementationSpec where

import Mybox.Aeson
import Mybox.Compute.Implementation
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "filterImplementation" $ do
    it "filters only top-level objects with Haskell implementation" $ do
      let input =
            toJSON
              [ object ["key" .= String "one"]
              , object ["key" .= String "two", "$implementation" .= String "haskell"]
              , object ["key" .= String "three", "$implementation" .= String "python"]
              ]
      filterImplementation input
        `shouldBe` toJSON
          [ object ["key" .= String "two"]
          ]
    it "passes through other data" $ do
      let shouldNotChange x = filterImplementation x `shouldBe` x
      shouldNotChange (String "hello")
      shouldNotChange $ toJSON [1 :: Int, 2, 3]
