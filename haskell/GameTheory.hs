module Algorithms.GameTheory (galeShapley) where
  import Prelude hiding (lookup)
  import Data.Maybe
  import Data.List (sortBy, elem)
  import Data.ListUtils (first)
  import Data.Map (Map, empty, member, lookup, insert, delete, adjust, keys)

  -- | The Stable Marriage Problem is the problem of finding a stable matching between two sets.
  -- A matching is a mapping from the elements of one set to the elements of the other.
  -- A matching is stable if the following does not hold:
  --  for every matching (a, b) there is anther matching (a', b') such that
  --  a prefers b' to b and b' prefers a to a'
  --
  -- This can be be expressed more intuitivly as:
  --   Given $n$ men and $n$ women, where each person has ranked the members of the opposite sex,
  --   from 1 to $n$, marry the men and women in monogamous and heterosexual couples such that
  --   noone would jilt their partner for somone else.
  stableMarriageProblem :: (Ord a, Ord b, Ord c1, Ord c2) => 
    [a] -> [b] -> (a -> b -> c1) -> (b -> a -> c2) -> Map a b
  stableMarriageProblem suitors reviewers sPrefs rPrefs = galeShapley sutPrf rvwPrf
    where
      sutPrf = foldl (prefOrder sPrefs reviewers) empty suitors
      rvwPrf = foldl (prefOrder rPrefs suitors)   empty reviewers
      prefOrder prefs ys acc xs = insert xs (orderPrefs prefs xs ys) acc
      orderPrefs prefs x = sortBy (\a b -> prefs x a `compare` prefs x b) 

  -- | Implementation of the Gale-Shapley algorithm that solves the Stable Marriage Problem.
  galeShapley :: (Ord a, Ord b) => Map a [b] -> Map b [a] -> Map a b
  galeShapley sPrefs rPrefs = (\(_,_,a,_) -> a) $ head $ dropWhile (\(a,_,_,_) -> (not.null) a) $ 
    iterate gs (keys sPrefs, sPrefs, empty, empty)
    where
     gs (sFree, sPrefs, engagesS, engagesR) = 
       if not $ w `member` engagesR then
         (ms, sPrefs', insert m w engagesS, insert w m engagesR)
       else
         if m == (fromJust $ first (fromJust $ lookup w rPrefs) [m,m']) then
           (m':ms, sPrefs', insert m w $ delete m' engagesS, insert w m engagesR)
         else
           (sFree, sPrefs', engagesS, engagesR)
       where
        sPrefs'  = adjust tail m sPrefs
        (m:ms) = sFree
        (w:_) = fromJust $ lookup m sPrefs
        m'     = fromJust $ lookup w engagesR

module Data.ListUtils where

  first :: Eq a => [a] -> [a] -> Maybe a
  first [] _ = Nothing
  first (x:xs) ys
    | x `elem` ys = Just x
    | otherwise   = first xs ys
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
module Learning.Perceptron (pla, applyHypothesis) where
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
  applyHypothesis w = (>=0) . (w <.>)

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
  pla :: Scalar a => [([a], Bool)] -> [a] -> Bool
  pla xys 
    | null xys = const True
    | (null . fst . head) xys  = const $ snd $ head xys
    | otherwise = applyHypothesis g . fromList . augment
    where -- g is the final hypothesis that correctly classifies all the pairs.
      g = snd $ head $ dropWhile (not.null.fst) $ iterate update (missed weights0, weights0)
      datalist = map (swap . (fmap $ fromList . augment) . swap) xys
      adjustHyp hypothesis (x, y) = hypothesis + (sigbool y) `scale` x
      sigbool True  = 1
      sigbool False = -1
      weights0 = 0 * (fst $ head $ datalist)
      misclassified w (x,y) = (y /=) $ applyHypothesis w x
      missed weights = [ d | d <- datalist, (misclassified weights) d ]
      update (misses, weights) = (missed weights', weights')
        where
          weights' = adjustHyp weights $ head misses

{-# LANGUAGE MultiParamTypeClasses #-}

--Probabilititic??
class Markov s a where
  succ :: s -> a -> s
  actions :: s -> [a]
  reward :: Fractional r => s -> r

data GridWorld

  
