module Main (main) where

import qualified Test.Data.FingerTree.RootMeasured.Strict
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Data" [
      testGroup "FingerTree" [
          testGroup "RootMeasured" [
              Test.Data.FingerTree.RootMeasured.Strict.tests
            ]
        ]
    ]
