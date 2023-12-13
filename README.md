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