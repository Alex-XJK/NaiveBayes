module Generator (
    generateSampleDataset,
    generateDataset,
    zipFeaturesToDataset
) where

import Data.List (zipWith6)
import System.Random (randomRs, mkStdGen)
import Data.Random.Normal (normals')

import Types

-- Function to generate a sample dataset
generateSampleDataset :: Dataset
generateSampleDataset = [
    (0, [1.0, 2.0, 3.0, 4.0, 5.0]),
    (1, [2.0, 3.0, 4.0, 5.0, 6.0]),
    (0, [3.0, 1.5, 2.5, 3.5, 4.5]),
    (2, [4.0, 2.5, 3.5, 4.5, 5.5])
  ]

-- Function Group to zip multiple feature arrays into the Dataset format
-- The number of features is 5
zipFeaturesToDataset_v5 :: [Int] -> [Double] -> [Double] -> [Double] -> [Double] -> [Double] -> Dataset
zipFeaturesToDataset_v5 = zipWith6 (\l f1 f2 f3 f4 f5 -> (l, [f1, f2, f3, f4, f5]))
-- Encapsulator of zipping 5 features into Dataset
zipFeaturesToDataset :: [Int] -> [[Double]] -> Dataset
zipFeaturesToDataset labels features = zipFeaturesToDataset_v5 labels (features !! 0) (features !! 1) (features !! 2) (features !! 3) (features !! 4)

-- Function to generate a 1-dimensional feature array with normal distribution
generateNormalFeature :: Int -> Double -> Double -> [Double]
generateNormalFeature size mean stdDev = take size $ normals' (mean, stdDev) gen
  where gen = mkStdGen 42

-- Function to generate an integer labels array with standard distribution
-- The labels are in the range [1, maxValue]
generateIntegerLabels :: Int -> Int -> [Int]
generateIntegerLabels size maxValue = take size $ randomRs (1, maxValue) gen
  where gen = mkStdGen 42

-- Function to generate a complete dataset
generateDataset :: Int -> Int -> [(Double, Double)] -> Dataset
generateDataset totalSize maxValue featureParams =
  let labels = generateIntegerLabels totalSize maxValue
      features = map (\(mean, stdDev) -> generateNormalFeature totalSize mean stdDev) featureParams
  in zipFeaturesToDataset labels features
