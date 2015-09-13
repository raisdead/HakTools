module Math.CombinatoricsProps (prop_sumFrom) where
  import Math.Combinatorics

  import Test.QuickCheck

  prop_sumFrom m n = 0 <= m && m <= n ==> sumFrom m n == (sum [m..n])
      
