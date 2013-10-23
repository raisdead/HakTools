{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

-- | A perceptron is a supervised learning algorithm for binary linear
-- classification.
--
-- A binay classificator maps a feature vector to a boolean value. A binary
-- linear classifier uses a linear combination of the vector's components to
-- classify the vector. The perceptron can be used as an online or offline
-- learning algorithm.
--
-- The credit for a perceptron is given to Frank Rosenbltt at Cornell 
-- Aeronautical Laboratory in 1957.
module Learning.Perceptron (pla) where
  import Data.Tuple (swap)
  import Foreign.Storable (Storable)

  import Data.Packed.Vector (Vector(..), fromList)
  import Numeric.Container ((<.>), scale, Product, Container)
  import Numeric.LinearAlgebra ()

  -- | A `Scalar` is a primitive quantity that stores a single quantity. It
  -- can be used as the component of a vector.
  type Scalar a = (Storable a, Num a, Num (Vector a), Product a, Ord a, Container Vector a)

  -- | Applies a vector of weights to an augmented feature vector.
  applyHypothesis :: Scalar a => Vector a -> Vector a -> Bool
  applyHypothesis w = (>0) . (w <.>)

  -- | Augments a feature vector with a bias term.
  augment :: Scalar a => [a] -> [a]
  augment = (1:)

  -- | The defacto iterative learning algorithm for training a perceptron. If 
  -- the data is linearly seperable, then the algorithm will produce a 
  -- function that will classify all the inputs correctly and hopefully new 
  -- inputs as well. If data is not linearly seperable, then the algorithm 
  -- will never terminate.
  --
  -- The algorithm repeated finds the first misclassified example and adjusts
  -- the hypothesis to classify it correctly. This is repeated  until all 
  -- (x, y) pairs are classified correctly.
  pla :: Scalar a => [([a], Bool)] -> ([a] -> Bool)
  pla xys = applyHypothesis g . fromList . augment
    where -- g is the final hypothesis that correctly classifies all the pairs.
      g = snd $ head $ dropWhile (not.null.fst) $ iterate update (missed weights0, weights0)
      datalist = map (swap . (fmap $ fromList . augment) . swap) xys
      adjustHyp hypothesis (x, y) = hypothesis + (sigbool y) `scale` x
      sigbool True  = 1
      sigbool False = -1
      weights0 = 0 * (fst $ head $ datalist)
      misclassified w (x,y) = (y /=) $ applyHypothesis w x
      missed weights = [ d | d <- datalist, (misclassified weights) d ]
      update (_, weights) = (mn, adjustHyp weights m1)
        where
          (m1:[], mn) = splitAt 1 $ missed weights

