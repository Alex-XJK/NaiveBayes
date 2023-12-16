module Generator (
    generateSampleDataset,
    generateRandomDataset,
    zipFeaturesToDataset,
    generateNormalFeature,
    generateIntegerLabels
) where

import Data.List (zipWith6)
import System.Random (randomRs, mkStdGen, random, StdGen)
import Data.Random.Normal (normals')

import Types

-- Function to generate a sample dataset
generateSampleDataset :: Dataset
generateSampleDataset =
  [(0, [1.0, 2.0, 3.0]), (1, [2.0, 3.0, 4.0]), (0, [1.0, 1.5, 2.5]), (1, [2.0, 2.5, 3.5])]

-- Function Group to zip multiple feature arrays into the Dataset format
zipFeaturesToDataset_v5 :: [Int] -> [Double] -> [Double] -> [Double] -> [Double] -> [Double] -> Dataset
zipFeaturesToDataset_v5 = zipWith6 (\l f1 f2 f3 f4 f5 -> (l, [f1, f2, f3, f4, f5]))

zipFeaturesToDataset :: [Int] -> [Double] -> [Double] -> [Double] -> [Double] -> [Double] -> Dataset
zipFeaturesToDataset = zipFeaturesToDataset_v5

-- Function to generate a 1-dimensional feature array with normal distribution
generateNormalFeature :: Int -> Double -> Double -> [Double]
generateNormalFeature size mean stdDev = take size $ normals' (mean, stdDev) gen
  where gen = mkStdGen 42

-- Function to generate an integer labels array with standard distribution
-- The labels are in the range [1, maxValue]
generateIntegerLabels :: Int -> Int -> [Int]
generateIntegerLabels size maxValue = take size $ randomRs (1, maxValue) gen
  where gen = mkStdGen 42

-- Function to generate a random dataset
generateRandomDataset :: Int -> Int -> Int -> Dataset
generateRandomDataset datasetSize labelCount featureCount =
  let gen = mkStdGen 42  -- Use a fixed seed for reproducibility
      labels = take datasetSize $ randomRs (0, labelCount - 1) gen
      features = replicate datasetSize $ replicate featureCount $ fst $ random (gen :: StdGen)
  in zip labels features
