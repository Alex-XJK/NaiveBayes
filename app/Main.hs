module Main (
  main
) where

import System.Environment (getArgs)
import Generator (generateDataset, generateDatasetParallel, readDataset)
import qualified Parallel as Par
import qualified Sequential as Seq

-- Main function
-- Run the program with the
--    -seq|-par [-file <path>]
-- e.g., stack run -- -par -file "./data/iris.tsv"
main :: IO ()
main = do
  args <- getArgs
  let totalSize = 1000000
      labelSize = 5
      featureParams = [(1.0, 0.25, 0.25), (-1.0, 0.5, 0.5), (80.0, 1.2, 0.0), (-2.0, 2.0, 2.0), (2, 1.0, 1.0)] -- [(mean, variance, noise)]
      kValue = 4
  case args of
    ["-par", "-file", path] -> runParallelFunctionsWithFile   path                              kValue
    ["-par"]                -> runParallelFunctions           totalSize labelSize featureParams kValue
    ["-seq", "-file", path] -> runSequentialFunctionsWithFile path                              kValue
    ["-seq"]                -> runSequentialFunctions         totalSize labelSize featureParams kValue
    _                       -> putStrLn "Invalid flag. Usage: -seq|-par [-file <path>]"

runParallelFunctionsWithFile :: String -> Int -> IO ()
runParallelFunctionsWithFile path kValue = do
  putStrLn "Running parallel functions with File Reading..."
  dataset <- readDataset path
  putStr "Read in Dataset: length = "
  print (length dataset)
  let result = Par.trainBestFeature kValue dataset
  putStrLn "Best Feature:"
  print result

runParallelFunctions :: Int -> Int -> [(Double, Double, Double)] -> Int -> IO ()
runParallelFunctions totalSize labelSize featureParams kValue = do
  putStrLn "Running parallel functions..."
  let dataset = generateDatasetParallel totalSize labelSize featureParams
  putStr "Generated Dataset: length = "
  print (length dataset)
  let result = Par.trainBestFeature kValue dataset
  putStrLn "Best Feature:"
  print result

runSequentialFunctionsWithFile :: String -> Int -> IO ()
runSequentialFunctionsWithFile path kValue = do
  putStrLn "Running sequential functions with File Reading..."
  dataset <- readDataset path
  putStr "Read in Dataset: length = "
  print (length dataset)
  let result = Seq.trainBestFeature kValue dataset
  putStrLn "Best Feature:"
  print result

runSequentialFunctions :: Int -> Int -> [(Double, Double, Double)] -> Int -> IO ()
runSequentialFunctions totalSize labelSize featureParams kValue = do
  putStrLn "Running sequential functions..."
  let dataset = generateDataset totalSize labelSize featureParams
  putStr "Generated Dataset: length = "
  print (length dataset)
  let result = Seq.trainBestFeature kValue dataset
  putStrLn "Best Feature:"
  print result

