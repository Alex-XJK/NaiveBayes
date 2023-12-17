module Utils
  ( calculateMeanAndStdDev,
    extractFeature,
    mergeFolds,
    splitIntoFolds,
    calculateLikelihood,
    calculateProduct,
    calculateStats,
  )
where

import Data.List.Split (chunksOf)
import Types

-- -- Function to calculate the mean and standard deviation of a list of values
-- calculateMeanAndStdDev :: [Double] -> (Double, Double)
-- calculateMeanAndStdDev xs =
--   let n = fromIntegral $ length xs
--       mean = sum xs / n
--       variance = sum (map (\x -> (x - mean) ^ 2) xs) / n
--       stdDev = sqrt variance
--   in (mean, stdDev)

-- Function to calculate the mean and standard deviation of a list of values
calculateMeanAndStdDev :: [Double] -> (Double, Double)
calculateMeanAndStdDev xs =
  let n = fromIntegral $ length xs
      mean = sum xs / n
      variance = sum (map (\x -> (x - mean) ^ 2) xs)
   in (mean, variance)

-- Function to calculate the statistics for a given label
calculateStats :: Label -> [Features] -> Double -> LabelStats
calculateStats label features prior =
  let labelValues = map (!! 0) features
      (meanVal, varVal) = calculateMeanAndStdDev labelValues
   in (label, meanVal, varVal, prior)

-- Calculate the likelihood of x for a normal distribution with mean mu and variance sigma2
calculateLikelihood :: Double -> Double -> Double -> Double
calculateLikelihood x mu sigma2 =
  let exponent = -((x - mu) ** 2) / (2 * sigma2)
      coefficient = 1 / sqrt (2 * pi * sigma2)
   in coefficient * exp exponent

-- Calculate the product of likelihood and prior for a given FeatureVector and LabelStats
calculateProduct :: Features -> LabelStats -> Double
calculateProduct feature (label, mean, variance, prior) =
  let likelihood = calculateLikelihood (head feature) mean variance
   in prior * likelihood

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