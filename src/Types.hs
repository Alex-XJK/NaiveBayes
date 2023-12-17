module Types
  ( Label,
    Features,
    LabeledFeatures,
    Dataset,
    Model,
    LabelStats,
  )
where

-- Type aliases for the dataset
type Label = Int

type Features = [Double]

type LabeledFeatures = (Label, Features) -- e.g., "(0, [1.0, 2.0, 3.0])"

type Dataset = [LabeledFeatures]

type LabelStats = (Label, Double, Double, Double) -- (Label, mean, variance, prior)

type Model = [LabelStats]
