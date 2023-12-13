module Lib
    ( crossValidation
    ) where

import Data.List (foldl', sortBy)
import Data.Map (Map, fromListWith, elems)
import Statistics.Distribution (density)
import Statistics.Distribution.Normal (normalDistr)

import Utils (calculateMeanAndStdDev, chunk)

-- Function to calculate the probability of a value given the normal distribution
probability :: Double -> (Double, Double) -> Double
probability x (mean, stdDev) = density (normalDistr mean stdDev) x

-- Function to predict the label for a given set of features
predict :: Map Double [(Double, Double)] -> [Double] -> Double
predict model features =
  let priorProbabilities = fmap (\distributions -> product $ map (\(f, dist) -> probability f dist) distributions) model
      posteriorProbabilities = zipWith (*) (elems priorProbabilities) features
      labelProbabilities = zip (elems priorProbabilities) posteriorProbabilities
      (label, _) = maximumBy (\(_, p1) (_, p2) -> compare p1 p2) labelProbabilities
  in label

-- Function to perform cross-validation
crossValidation :: Int -> [(Double, [Double])] -> Double
crossValidation k dataset =
  let chunks = chunk (length dataset `div` k) dataset
      accuracies = map (\validationSet -> let trainingSet = filter (`notElem` validationSet) dataset
                                              model = trainNaiveBayes trainingSet
                                              correctPredictions = length $ filter (\(label, features) -> predict model features == label) validationSet
                                          in fromIntegral correctPredictions / fromIntegral (length validationSet)) chunks
  in sum accuracies / fromIntegral k
