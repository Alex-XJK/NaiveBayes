module Sequential
  ( trainModel,
    predictSingleVector,
    predict,
    calculateErrorRate,
    trainAndValidate,
    splitData,
    kFoldCrossValidation,
    evaluateFeature,
    findBestFeature,
    trainBestFeature,
  )
where

import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import GHC.OldList (genericSplitAt)
import Types (Dataset, ErrorRate, Features, Label, LabeledFeatures, Model)
import Utils (averageErrorRates, calculateProduct, calculateStats, extractFeature, extractFeatures, extractLabels)

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

-- Function to calculate the accuracy of the model
calculateErrorRate :: [Label] -> [Label] -> ErrorRate
calculateErrorRate predictedLabels actualLabels =
  let totalLabels = length actualLabels
      incorrectLabels = filter (uncurry (/=)) (zip predictedLabels actualLabels)
      numIncorrect = length incorrectLabels
   in fromIntegral numIncorrect / fromIntegral totalLabels

-- Function to train and evaluate the model
trainAndValidate :: ([LabeledFeatures], [LabeledFeatures]) -> ErrorRate
trainAndValidate tvPair =
  let trainingData = fst tvPair
      validationData = snd tvPair
      model = trainModel trainingData
      predicted = predict model (extractFeatures validationData)
   in calculateErrorRate predicted (extractLabels validationData)

-- Split the dataset into training and validation sets at index i*len(dataset)/k
splitData :: Int -> Int -> Dataset -> ([LabeledFeatures], [LabeledFeatures])
splitData i k dataset =
  let totalSize = length dataset
      (validationStart, validationEnd) = (i * totalSize `div` k, (i + 1) * totalSize `div` k)
      (validation, rest) = genericSplitAt (validationEnd - validationStart) (drop validationStart dataset)
      training = take validationStart dataset ++ rest
   in (training, validation)

-- Perform k-fold cross-validation and calculate the average error rate
kFoldCrossValidation :: Int -> Dataset -> ErrorRate
kFoldCrossValidation k dataset =
  let errorRates = map (\i -> trainAndValidate (splitData i k dataset)) [0 .. k - 1]
  in averageErrorRates errorRates

-- Perform k-fold cross-validation for a single feature
evaluateFeature :: Int -> Int -> Dataset -> ErrorRate
evaluateFeature k featureIndex dataset =
  let featureOnly = extractFeature dataset featureIndex
   in kFoldCrossValidation k featureOnly

-- Find the feature with the minimum average error rate
findBestFeature :: Int -> Dataset -> (Int, ErrorRate)
findBestFeature k dataset =
  let numFeatures = length (snd (head dataset))
      errorRates = map (\idx -> (idx, evaluateFeature k idx dataset)) [0 .. numFeatures - 1]
   in minimumBy (comparing snd) errorRates

-- Train on the best feature found by k-fold cross-validation
trainBestFeature :: Int -> Dataset -> Model
trainBestFeature k dataset =
  let (bestFeature, _) = findBestFeature k dataset
      bestFeatureOnly = extractFeature dataset bestFeature
   in trainModel bestFeatureOnly