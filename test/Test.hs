module Test where
  import Test.QuickCheck

  import Learning.PerceptronProps
  import Math.CombinatoricsProps

  main = do
    MapM_ (quickCheckWith stdArgs {maxSuccess = 1000}) [prop_plaStable, prop_sumFrom]
