module Main (
  main
) where

import System.Environment (getArgs)
import Generator (generateDataset, generateDatasetParallel)
import qualified Parallel as Par
import qualified Sequential as Seq

-- Main function
-- Run the program with the
--    "-par" flag to run the parallel functions
--    "-seq" flag to run the sequential functions
-- e.g., stack run -- -par
main :: IO ()
main = do
  args <- getArgs
  let totalSize = 1000000
      labelSize = 5
      featureParams = [(1.0, 0.25, 0.25), (-1.0, 0.5, 0.5), (80.0, 1.2, 0.0), (-2.0, 2.0, 2.0), (2, 1.0, 1.0)] -- [(mean, variance, noise)]
      kValue = 4
  case args of
    ["-par"] -> runParallelFunctions totalSize labelSize featureParams kValue
    ["-seq"] -> runSequentialFunctions totalSize labelSize featureParams kValue
    _        -> putStrLn "Invalid flag. Use either -par or -seq."

runParallelFunctions :: Int -> Int -> [(Double, Double, Double)] -> Int -> IO ()
runParallelFunctions totalSize labelSize featureParams kValue = do
  putStrLn "Running parallel functions..."
  let dataset = generateDatasetParallel totalSize labelSize featureParams
  putStr "Generated Dataset: length = "
  print (length dataset)
  let result = Par.trainBestFeature kValue dataset
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

