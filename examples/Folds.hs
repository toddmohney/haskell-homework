module Folds where

  maximum' :: (Ord a) => [a] -> a
  maximum' = foldl1 (\acc x -> if x > acc then x else acc)

  minimum' :: (Ord a) => [a] -> a
  minimum' = foldl1 (\acc x -> if x < acc then x else acc)

  reverse' :: [a] -> [a]
  reverse' = foldl (\acc x -> x : acc) []

  reverse'' :: [a] -> [a]
  reverse'' = foldr (\x acc -> acc ++ [x]) []

  product' :: (Num a) => [a] -> a
  product' = foldl (\acc x -> x * acc) 1

  product'' :: (Num a) => [a] -> a
  product'' = foldr (\x acc -> x * acc) 1

  filter' :: (a -> Bool) -> [a] -> [a]
  filter' f = foldl (\acc x -> if (f x) then acc ++ [x] else acc) []

  filter'' :: (a -> Bool) -> [a] -> [a]
  filter'' f = foldr (\x acc -> if (f x) then x:acc else acc) []

  head' :: [a] -> a
  head' = foldl1 (\x _ -> x)

  last' :: [a] -> a
  last' = foldr1 (\_ x -> x)


