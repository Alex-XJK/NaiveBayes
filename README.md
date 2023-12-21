# NaiveBayes
Implementing a Parallel Naive Bayes Classifier Using the Haskell Language

## what data looks like
| color  | length  | width  | price  |  label |
|---|---|---|---|---|
|  R |  10 |  10 |  100 | 1  |
|  G |  20 |  20 |  200 | 2  |
|  B |  30 |  30 |  300 | 3  |

## pseudocode
```
for feature in features:
    for possible train-validation split:
        use training data (this feature) to train model
        use validation data (this feature) to test model
        get error rate
    average all error rates for this feature
select the feature with the lowest error rate
```

## Usage
Use `stack build` to build the program.  
Run the program with `stack run -- [command line arguments]`  
Where the command line arguments can be:
- `-h` to see the usage message
- `-seq` to run the sequential functions with randomly generated dataset
- `-seq -file <data_path> <test_path>` to run the sequential functions with your data and test files
- `-par` to run the parallel functions with randomly generated dataset
- `-par -file <data_path> <test_path>` to run the parallel functions with your data and test files
Please notice that, you files have to be in a space/tab-separated file, in the format of:
```
label   feature1    feature2 ...
label   feature1    feature2 ...
label   feature1    feature2 ...
...
```
We have included a sample iris data in our `./data/` directory, for your reference.
To run with that, use command `stack run -- -par -file ./data/iris.tsv ./data/iris_test.tsv`