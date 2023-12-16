module Main (
  main
) where

import Generator(sampleDataset, plainDataset, generateDataset, readDataset)

main :: IO ()
main = do
  let sDataset = sampleDataset
  putStrLn "Sample Dataset:"
  print sDataset

  let labels = [0, 1, 0, 2] :: [Int]
      features1 = [1.0, 2.0, 3.0, 4.0] :: [Double]
      features2 = [2.0, 3.0, 1.5, 2.5] :: [Double]
      features3 = [3.0, 4.0, 2.5, 3.5] :: [Double]
      features4 = [4.0, 5.0, 3.5, 4.5] :: [Double]
      features5 = [5.0, 6.0, 4.5, 5.5] :: [Double]
      pDataset = plainDataset labels [features1, features2, features3, features4, features5]
  putStrLn "Zip 5 features to dataset:"
  print pDataset

  let totalSize = 10
      maxValue = 5
      featureParams = [(1.0, 0.5), (10.0, 2.0), (10.0, 5.0), (50.0, 10.0), (30.0, 5.0)]
      gDataset = generateDataset totalSize maxValue featureParams
  putStrLn "Generated Dataset:"
  print gDataset

  let filePath = "./data/test.tsv"
  rDataset <- readDataset filePath
  putStrLn "Read Dataset:"
  print rDataset