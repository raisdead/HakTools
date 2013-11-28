module Data.ListUtils where

  first :: Eq a => [a] -> [a] -> Maybe a
  first [] _ = Nothing
  first (x:xs) ys
    | x `elem` ys = Just x
    | otherwise   = first xs ys
