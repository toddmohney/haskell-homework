module MyStuff where

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
