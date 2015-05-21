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


data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node a left right) = 1 + max (treeHeight left) (treeHeight right)


data Direction = LeftTurn
               | RightTurn
               | Straight
               deriving (Show)

data Point = Point Int Int

-- using Graham Scan algorithm
whichWay :: Point -> Point -> Point -> Direction
whichWay (Point x1 y1) (Point x2 y2) (Point x3 y3)
  | crossProduct == 0     = Straight
  | crossProduct > 0      = LeftTurn
  | otherwise             = RightTurn
    where crossProduct = ((x2 - x1) * (y3 - y1)) - ((y2 - y1) * (x3 - x1)) 


turnByTurn :: [Point] -> [Direction]
turnByTurn ([]) = []
turnByTurn (_:[]) = []
turnByTurn (_:_:[]) = []
turnByTurn (a:b:c:[]) = [(whichWay a b c)]
turnByTurn (a:b:c:xs) = ((turnByTurn [a, b, c]) ++ turnByTurn (b:c:xs))

