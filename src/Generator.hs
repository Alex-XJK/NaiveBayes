module Generator (
    generateSampleDataset,
    generateRandomDataset
) where

import System.Random (randomRs, newStdGen, mkStdGen, random, StdGen)

import Types

-- Function to generate a sample dataset
generateSampleDataset :: Dataset
generateSampleDataset =
  [(0, [1.0, 2.0, 3.0]), (1, [2.0, 3.0, 4.0]), (0, [1.0, 1.5, 2.5]), (1, [2.0, 2.5, 3.5])]

-- Function to generate a random dataset
generateRandomDataset :: Int -> Int -> Int -> Dataset
generateRandomDataset datasetSize labelCount featureCount =
  let gen = mkStdGen 42  -- Use a fixed seed for reproducibility
      labels = take datasetSize $ randomRs (0, labelCount - 1) gen
      features = replicate datasetSize $ replicate featureCount $ fst $ random (gen :: StdGen)
  in zip labels features
