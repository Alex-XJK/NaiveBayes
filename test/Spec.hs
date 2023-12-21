import Generator (generateDataset)
import qualified Parallel as Par

main :: IO ()
main = do
  let   dataset = generateDataset 100000 5 [(5.0, 0.25), (5.0, 0.25), (5.0, 0.25), (5.0, 0.25), (5.0, 0.25)] [0.25, 0.25, 0, 0.25, 0.25]
        (model, _) = Par.trainBestFeature 5 dataset
  putStr "Generator... "
  putStrLn $ if length dataset == 100000 then "OK" else "FAIL!"
  putStr "Model... "
  putStrLn $ if length model == 5 then "OK" else "FAIL!"
