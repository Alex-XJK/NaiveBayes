module Sequential
  ( compareFeature,
  )
where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Types (Dataset, Features, Label, LabelStats, Model)
import Utils (calculateLikelihood, calculateMeanAndStdDev, calculateProduct, calculateStats, extractFeature, mergeFolds, splitIntoFolds)

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

-- Train a model
trainModel :: Dataset -> Model
trainModel samples =
  let totalSamples = fromIntegral $ length samples
      labelSet = Set.fromList $ map fst samples
      labelStats =
        map
          ( \label ->
              let labelSamples = filter ((== label) . fst) samples
                  labelCount = fromIntegral $ length labelSamples
               in calculateStats label (map snd labelSamples) (labelCount / totalSamples)
          )
          (Set.toList labelSet)
   in labelStats

-- Predict label for a single FeatureVector using the Model
predictSingleVector :: Model -> Features -> Label
predictSingleVector model feature =
  let calculateProductsForFeature = map (calculateProduct feature) model
      getBestLabelIdx productValues = fst $ maximumBy (compare `on` snd) $ zip [0 ..] productValues
      bestLabelIndex = getBestLabelIdx calculateProductsForFeature
      bestLabel = (\(label, _, _, _) -> label) (model !! bestLabelIndex)
   in bestLabel

-- Predict labels for a list of FeatureVectors using the Model
predict :: Model -> [Features] -> [Label]
predict model = map (predictSingleVector model)