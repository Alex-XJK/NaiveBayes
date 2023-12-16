module Types (
    Label,
    Features,
    LabeledFeatures,
    Dataset,
    Model
) where

-- Type aliases for the dataset
type Label = Int

type Features = [Double]

type LabeledFeatures = (Label, Features) -- e.g., "(0, [1.0, 2.0, 3.0])"

type Dataset = [LabeledFeatures]

type Model = [(Double, Double)] -- [(mean, variance)]
