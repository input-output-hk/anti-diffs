module Main (main) where

import           Test.Tasty

import qualified Test.Data.Map.AntiDiff.Strict
import qualified Test.Data.Map.Diff.Strict

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Data" [
      testGroup "Map" [
          testGroup "Diff" [
              Test.Data.Map.AntiDiff.Strict.tests
            , Test.Data.Map.Diff.Strict.tests
            ]
        ]
    ]
