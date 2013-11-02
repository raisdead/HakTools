module Test where
  import Test.QuickCheck

  import Learning.PerceptronProps

  main = do
    quickCheckWith stdArgs {maxSuccess = 1000} prop_plaStable
