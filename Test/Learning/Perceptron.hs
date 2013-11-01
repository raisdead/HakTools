module Test.Learning.Perceptron (prop_plaStable) where
  import Data.Packed.Vector (fromList)
  import Test.QuickCheck

  import Learning.Perceptron

  -- | A data structure that represents validating if a perceptron is stable.
  data PerceptronStable = PerceptronStable [([Double],Bool)] deriving Show
  instance Arbitrary PerceptronStable where
    arbitrary = do
      n <- choose (1,10) -- Pick the dimensionality
      m <- choose (1,100) -- Pick the number of training vectors
      f <- vector n :: Gen [Double] -- Pick the function that represents the world
      xs <-  mapM vector $ take m $ repeat n :: Gen [[Double]] -- Pick some vectors from the world.
      let xs' = map (\x -> (x, applyHypothesis (fromList (1:f)) (fromList (1:x)))) xs
      return $ PerceptronStable xs'

  prop_plaStable (PerceptronStable xys) = and $ map (\(x,y) -> y == p x) xys
    where
      p = pla xys
      
