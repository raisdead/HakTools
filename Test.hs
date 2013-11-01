import Test.QuickCheck

import Test.Learning.Perceptron

main = do
  quickCheckWith stdArgs {maxSuccess = 1000} prop_plaStable
