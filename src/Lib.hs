module Lib
    ( crossValidation
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
import Utils (calculateMeanAndStdDev, chunk)
-- Function to perform cross-validation
crossValidation :: Int -> [(Double, [Double])] -> Double
