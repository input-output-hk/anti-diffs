module Main (main) where

import qualified Test.Data.Map.Diff.Strict
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Data" [
      testGroup "Map" [
          testGroup "Diff" [
                Test.Data.Map.Diff.Strict.tests
            ]
        ]
    ]
