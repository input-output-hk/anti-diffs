module Main (main) where

import qualified Test.Data.Map.Diff.Simple.Strict
import qualified Test.Data.Map.Diff.Strict
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "diff-containers" [
      Test.Data.Map.Diff.Simple.Strict.tests
    , Test.Data.Map.Diff.Strict.tests
    ]
