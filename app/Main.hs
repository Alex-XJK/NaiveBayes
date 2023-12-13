module Main (main) where

import Lib

main :: IO ()
main = do
  let dataset = [(0.0, [1.0, 2.0, 3.0]), (1.0, [2.0, 3.0, 4.0]), (0.0, [1.0, 1.5, 2.5]), (1.0, [2.0, 2.5, 3.5])]
      k = 4     -- Number of folds for cross-validation
  putStrLn $ "Cross-validation accuracy: " ++ show (crossValidation k dataset)

    -- labels = [0, 1, 0, 1]
    -- feature1 = [1.0, 2.0, 1.0, 2.0]
    -- feature2 = [2.0, 3.0, 1.5, 2.5]
    -- feature3 = [3.0, 4.0, 2.5, 3.5]