module Main (
  main
) where

import System.Environment (getArgs)
import Generator (generateDataset, generateDatasetParallel, readDataset)
import qualified Parallel as Par
import qualified Sequential as Seq

-- Main function
-- Run the program with the
--    -seq|-par [-file <path> <test_path>]
-- e.g., stack run -- -par -file "./data/iris.tsv"
main :: IO ()
main = do
  args <- getArgs
  let totalSize = 1000000
      testSize = 100
      labelSize = 5
      featureParams = [(1.0, 0.25), (-1.0, 0.5), (80.0, 1.2), (-2.0, 2.0), (2, 1.0)] -- [(mean, variance)]
      noiseArray = [1, 2, 0, 4, 5]
      kValue = 4
  case args of
    ["-par", "-file", path, testpath] -> runParallelFunctionsWithFile   path                                          kValue testpath
    ["-par"]                          -> runParallelFunctions           totalSize labelSize featureParams noiseArray  kValue testSize
    ["-seq", "-file", path, testpath] -> runSequentialFunctionsWithFile path                                          kValue testpath
    ["-seq"]                          -> runSequentialFunctions         totalSize labelSize featureParams noiseArray  kValue testSize
    _                                 -> putStrLn "Invalid flag. Usage: -seq|-par [-file <path> <test_path>]"

runParallelFunctionsWithFile :: String -> Int -> String -> IO ()
runParallelFunctionsWithFile path kValue testpath = do
  putStrLn "Running parallel functions with File Reading..."
  putStr "Read in Dataset: length = "
  dataset <- readDataset path
  print (length dataset)
  putStr "Read in Test Dataset: length = "
  testDataset <- readDataset testpath
  print (length testDataset)
  let (model, idx) = Par.trainBestFeature kValue dataset
  putStr "Best Feature: "
  print idx
  putStrLn "Error rate using the best feature on testing data:"
  let errorRate = Par.getBestErrorRate idx testDataset model
  print errorRate

runParallelFunctions :: Int -> Int -> [(Double, Double)] -> [Double] -> Int -> Int -> IO ()
runParallelFunctions totalSize labelSize featureParams noiseArray kValue testSize = do
  putStrLn "Running parallel functions..."
  let dataset = generateDatasetParallel totalSize labelSize featureParams noiseArray
      testDataset = generateDataset testSize labelSize featureParams noiseArray
  putStr "Generated Dataset: length = "
  print (length dataset)
  putStr "Generated Test Dataset: length = "
  print (length testDataset)
  let (model, idx) = Par.trainBestFeature kValue dataset
  putStr "Best Feature: "
  print idx
  putStrLn "Error rate using the best feature on testing data:"
  let errorRate = Par.getBestErrorRate idx testDataset model
  print errorRate

runSequentialFunctionsWithFile :: String -> Int -> String -> IO ()
runSequentialFunctionsWithFile path kValue testpath = do
  putStrLn "Running sequential functions with File Reading..."
  putStr "Read in Dataset: length = "
  dataset <- readDataset path
  print (length dataset)
  putStr "Read in Test Dataset: length = "
  testDataset <- readDataset testpath
  print (length testDataset)
  let (model, idx) = Seq.trainBestFeature kValue dataset
  putStr "Best Feature: "
  print idx
  putStrLn "Error rate using the best feature on testing data:"
  let errorRate = Seq.getBestErrorRate idx testDataset model
  print errorRate

runSequentialFunctions :: Int -> Int -> [(Double, Double)] -> [Double] -> Int -> Int -> IO ()
runSequentialFunctions totalSize labelSize featureParams noiseArray kValue testSize = do
  putStrLn "Running sequential functions..."
  let dataset = generateDataset totalSize labelSize featureParams noiseArray
      testDataset = generateDataset testSize labelSize featureParams noiseArray
  putStr "Generated Dataset: length = "
  print (length dataset)
  putStr "Generated Test Dataset: length = "
  print (length testDataset)
  let (model, idx) = Seq.trainBestFeature kValue dataset
  putStr "Best Feature: "
  print idx
  putStrLn "Error rate using the best feature on testing data:"
  let errorRate = Seq.getBestErrorRate idx testDataset model
  print errorRate


