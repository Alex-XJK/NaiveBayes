module Main (
  main
) where

import Generator (generateSampleDataset, generateRandomDataset)

main :: IO ()
main = do
  let sampleDataset = generateSampleDataset
  putStrLn "Sample Dataset:"
  print sampleDataset

  let randomDataset = generateRandomDataset 100 2 5  -- Example: Dataset size = 100, Label count = 2, Feature count = 5
  putStrLn "\nRandomly Generated Dataset:"
  print randomDataset