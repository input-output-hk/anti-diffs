module Main (main) where

import           Test.Data.Semigroupoid.Simple (tests)
import           Test.Tasty

main :: IO ()
main = defaultMain $
  testGroup "Data" [
      Test.Data.Semigroupoid.Simple.tests
    ]
