module Math.Combinatorics where

  -- | Computes the sum from m to n. O(1)
  sumFrom :: Integral a => a -> a -> a
  sumFrom m n = (n + m)*(n - m + 1) `div` 2
