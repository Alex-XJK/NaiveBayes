module Main (
  main
) where

import Generator(generateSampleDataset, zipFeaturesToDataset, generateNormalFeature, generateIntegerLabels)

main :: IO ()
main = do
  let sampleDataset = generateSampleDataset
  putStrLn "Sample Dataset:"
  print sampleDataset

  let labels = [0, 1, 0, 2] :: [Int]
      features1 = [1.0, 2.0, 3.0, 4.0] :: [Double]
      features2 = [2.0, 3.0, 1.5, 2.5] :: [Double]
      features3 = [3.0, 4.0, 2.5, 3.5] :: [Double]
      features4 = [4.0, 5.0, 3.5, 4.5] :: [Double]
      features5 = [5.0, 6.0, 4.5, 5.5] :: [Double]
      dataset = zipFeaturesToDataset labels features1 features2 features3 features4 features5
  putStrLn "Zip 5 features to dataset:"
  print dataset

  let featureN = generateNormalFeature 20 100.0 10.0
  putStrLn "Normal Distributed feature is:"
  print featureN

  let labelN = generateIntegerLabels 20 4
  putStrLn "Normal Distributed label is:"
  print labelN