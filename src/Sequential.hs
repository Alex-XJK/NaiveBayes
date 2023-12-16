module Sequential (
    compareFeature
) where

import Data.List (maximumBy)
import Data.Ord (comparing)

import Types (Dataset, Model)
import Utils (calculateMeanAndStdDev, extractFeature, mergeFolds, splitIntoFolds)

-- Function to compare features
compareFeature :: Dataset -> Int -> Double
compareFeature dataset k =
  let indices = [0 .. length (snd (head dataset)) - 1]
      accuracies = map (\idx -> splitTest (extractFeature dataset idx) k) indices
  in maximum accuracies

-- Function to split test
splitTest :: Dataset -> Int -> Double
splitTest dataset k = undefined
-- splitTest dataset k =
--   let folds = splitIntoFolds dataset k
--       accuracies = map (\i -> trainAndEvaluate (mergeFolds (take i folds) (drop (i + 1) folds))) [0..k-1]
--   in maximumBy (comparing fst) accuracies

-- Function to train and evaluate the model
trainAndEvaluate :: Dataset -> (Double, Model)
trainAndEvaluate dataset = undefined

-- Function for making predictions
prediction :: Model -> Dataset -> Double
prediction model dataset = undefined
