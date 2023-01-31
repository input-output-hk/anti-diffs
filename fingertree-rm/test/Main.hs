module Main (main) where

import           Test.Tasty

import qualified Test.Data.FingerTree.RootMeasured.Strict

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
