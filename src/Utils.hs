module Utils (
    calculateMeanAndStdDev,
    extractFeature,
    mergeFolds,
    splitIntoFolds
) where

import Data.List.Split (chunksOf)

import Types

-- Function to calculate the mean and standard deviation of a list of values
calculateMeanAndStdDev :: [Double] -> (Double, Double)
calculateMeanAndStdDev xs =
  let n = fromIntegral $ length xs
      mean = sum xs / n
      variance = sum (map (\x -> (x - mean) ^ 2) xs) / n
      stdDev = sqrt variance
  in (mean, stdDev)

-- Function to extract a feature from a dataset
extractFeature :: Dataset -> Int -> Dataset
extractFeature dataset idx = map (\(label, features) -> (label, [features !! idx])) dataset

-- Function to merge folds for training
mergeFolds :: [Dataset] -> Dataset
mergeFolds = concat

-- Function to split dataset into k folds
splitIntoFolds :: Dataset -> Int -> [Dataset]
splitIntoFolds dataset k =
  let (foldSize, remainder) = length dataset `quotRem` k
      initialFoldSize = foldSize + if remainder > 0 then 1 else 0
  in chunksOf initialFoldSize dataset