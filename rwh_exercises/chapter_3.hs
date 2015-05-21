module MyStuff where

import Data.Ord (comparing)
import Data.List (sortBy)

-- 1,2
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)


-- 3
myMean :: [Int] -> Float
myMean x = (fromIntegral (sum x)) / (fromIntegral (myLength x))

-- 4
toPalendrome :: [a] -> [a]
toPalendrome x = x ++ (reverse x)


-- 5
isPalendrome :: (Eq a) => [a] -> Bool
isPalendrome x = x == (reverse x)


-- 6
sortLists :: [[a]] -> [[a]]
sortLists x = sortBy (comparing length) x

sortLists2 :: [[a]] -> [[a]]
sortLists2 x = sortBy compareLength x
  where compareLength a b = compare (length a) (length b)


-- 7
intersperse :: a -> [[a]]-> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x 
intersperse sep (x:xs) = x ++ [sep] ++ intersperse sep xs

