module Main (main) where

import           Bench.Data.FingerTree.RootMeasured.Strict (benchmarks)
import           Test.Tasty.Bench

main :: IO ()
main = defaultMain [
    bgroup "Bench" [
        bgroup "Data" [
            bgroup "FingerTree" [
                bgroup "RootMeasured" [
                    benchmarks
                  ]
              ]
          ]
      ]
  ]
