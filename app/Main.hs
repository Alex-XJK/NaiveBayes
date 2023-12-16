module Main (
  main
) where

import Generator

main :: IO ()
main = do
  let totalSize = 10
      maxValue = 5
      featureParams = [(1.0, 0.5), (10.0, 2.0), (10.0, 5.0), (50.0, 10.0), (30.0, 5.0)]
      gDataset = generateDataset totalSize maxValue featureParams
  putStrLn "Generated Dataset:"
  print gDataset
