module Generator (
    sampleDataset,
    plainDataset,
    generateDataset,
    generateDatasetParallel,
    readDataset
) where

import Data.List (zipWith6)
import System.Random (randomRs, mkStdGen)
import Data.Random.Normal (normals')
import System.IO ()

import Types

import Control.Parallel.Strategies

-- [Public] Retrieve a fixed sample dataset
sampleDataset :: Dataset
sampleDataset = [
    (0, [1.0, 2.0, 3.0, 4.0, 5.0]),
    (1, [2.0, 3.0, 4.0, 5.0, 6.0]),
    (0, [3.0, 1.5, 2.5, 3.5, 4.5]),
    (2, [4.0, 2.5, 3.5, 4.5, 5.5])
  ]

-- -- Function Group to zip multiple feature arrays into the Dataset format

-- The number of features is 5
zipFeaturesToDataset_v5 :: [Int] -> [Double] -> [Double] -> [Double] -> [Double] -> [Double] -> Dataset
zipFeaturesToDataset_v5 = zipWith6 (\l f1 f2 f3 f4 f5 -> (l, [f1, f2, f3, f4, f5]))

-- [Public] [Encapsulator] Convert a list of labels and a list of feature arrays into a Dataset
plainDataset :: [Int] -> [[Double]] -> Dataset
plainDataset labels features = zipFeaturesToDataset_v5 labels (head features) (features !! 1) (features !! 2) (features !! 3) (features !! 4)

-- -- Function Group to zip multiple feature arrays into the Dataset format

-- Function to generate a 1-dimensional feature array with normal distribution
generateNormalFeature :: Int -> Double -> Double -> [Double]
generateNormalFeature size mean stdDev = take size $ normals' (mean, stdDev) gen
  where gen = mkStdGen 42

-- Function to generate an integer labels array with standard distribution
-- The labels are in the range [1, maxValue]
generateIntegerLabels :: Int -> Int -> [Int]
generateIntegerLabels size maxValue = take size $ randomRs (1, maxValue) gen
  where gen = mkStdGen 42

-- [Public] Generate a complete dataset with given parameters
generateDataset :: Int -> Int -> [(Double, Double)] -> Dataset
generateDataset totalSize maxValue featureParams =
  let labels = generateIntegerLabels totalSize maxValue
      features = map (uncurry (generateNormalFeature totalSize)) featureParams
  in plainDataset labels features

-- [Parallel] Parallel version of generateDataset
-- [Public] Generate a complete dataset with given parameters
generateDatasetParallel :: Int -> Int -> [(Double, Double)] -> Dataset
generateDatasetParallel totalSize maxValue featureParams =
  let labels = runEval $ parList rpar $ generateIntegerLabels totalSize maxValue
      features = parMap rpar (uncurry (generateNormalFeature totalSize)) featureParams
  in plainDataset labels features

-- Function to parse a row
parseRow :: [String] -> LabeledFeatures
parseRow [] = error "Empty row"
parseRow (labelStr : featureStrs) = (read labelStr, map read featureStrs)

-- [Public] Read a dataset from a TSV file
readDataset :: FilePath -> IO Dataset
readDataset filePath = do
  content <- readFile filePath
  return $ map (parseRow . words) (lines content)