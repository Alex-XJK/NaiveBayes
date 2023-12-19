module Utils
  ( calculateMeanAndStdDev,
    extractFeature,
    mergeFolds,
    splitIntoFolds,
    calculateLikelihood,
    calculateProduct,
    calculateStats,
    extractFeatures,
    extractLabels,
    averageErrorRates,
  )
where

import Data.List.Split (chunksOf)
import Types
  ( Dataset,
    ErrorRate,
    Features,
    Label,
    LabelStats,
    LabeledFeatures,
  )

-- Function to calculate the mean and standard deviation of a list of values
calculateMeanAndStdDev :: [Double] -> (Double, Double)
calculateMeanAndStdDev xs =
  let n = fromIntegral $ length xs
      mean = sum xs / n
      variance = sum (map (\x -> (x - mean) ^ (2 :: Integer)) xs) / n
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
  let myexponent = -((x - mu) ** 2) / (2 * sigma2)
      coefficient = 1 / sqrt (2 * pi * sigma2)
   in coefficient * exp myexponent

-- Calculate the product of likelihood and prior for a given FeatureVector and LabelStats
calculateProduct :: Features -> LabelStats -> Double
calculateProduct feature (_, mean, variance, prior) =
  let likelihood = calculateLikelihood (head feature) mean variance
   in prior * likelihood

-- Function to extract a feature from a dataset
extractFeature :: Dataset -> Int -> Dataset
extractFeature dataset idx = map (\(label, features) -> (label, [features !! idx])) dataset

-- Function to separate labels from features
extractFeatures :: [LabeledFeatures] -> [Features]
extractFeatures = map snd

-- Function to separate labels from features
extractLabels :: [LabeledFeatures] -> [Label]
extractLabels = map fst

-- Function to calculate the average error rate
averageErrorRates :: [ErrorRate] -> ErrorRate
averageErrorRates errorRates =
  let total = sum errorRates
      count = fromIntegral (length errorRates)
   in total / count

-- Function to merge folds for training
mergeFolds :: [Dataset] -> Dataset
mergeFolds = concat

-- Function to split dataset into k folds
splitIntoFolds :: Dataset -> Int -> [Dataset]
splitIntoFolds dataset k =
  let (foldSize, remainder) = length dataset `quotRem` k
      initialFoldSize = foldSize + if remainder > 0 then 1 else 0
   in chunksOf initialFoldSize dataset