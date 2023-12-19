module Main (
  main
) where

import Generator
import Sequential (trainBestFeature)

main :: IO ()
main = do
  let totalSize = 100000
      maxValue = 5
      featureParams = [(1.0, 0.5), (10.0, 2.0), (10.0, 5.0), (50.0, 10.0), (30.0, 5.0)]
      gDataset_p = generateDatasetParallel totalSize maxValue featureParams
  putStrLn "Generated Dataset with Parallel:"
  print (length gDataset_p)

  let result = trainBestFeature 4 gDataset_p
  putStrLn "Best Feature:"
  print result

