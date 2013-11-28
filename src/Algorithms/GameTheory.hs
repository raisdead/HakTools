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

