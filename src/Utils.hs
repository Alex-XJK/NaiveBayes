module Utils (
    calculateMeanAndStdDev,
    chunk
) where

import Data.List (transpose)

-- Function to calculate the mean and standard deviation of a list of values
calculateMeanAndStdDev :: [Double] -> (Double, Double)
calculateMeanAndStdDev xs =
  let n = fromIntegral $ length xs
      mean = sum xs / n
      variance = sum (map (\x -> (x - mean) ^ 2) xs) / n
      stdDev = sqrt variance
  in (mean, stdDev)

-- Function to split a list into chunks for cross-validation
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)
