{-# OPTIONS_GHC -Wall #-}
module Golf where

  skips :: [a] -> [[a]]
  skips xs = map (`everyNth` xs) [1..(length xs)]

  everyNth :: Int -> [a] -> [a]
  everyNth num xs 
    | length xs >= num = (last . take num $ xs) : everyNth num (drop num xs)
    | otherwise = []

  type IndexResultSource = (Int,[Int],[Int])

  localMaximum :: [Int] -> [Int]
  localMaximum list = parseTuple $ foldr filterMaximum (0,[],list) list
    where 
      parseTuple :: IndexResultSource -> [Int]
      parseTuple (_,res,_) = res

      filterMaximum :: Int -> IndexResultSource -> IndexResultSource
      filterMaximum _ (idx,res,src)
        | idx == 0                                   = (idx+1,res,src)
        | idx == length src - 1                      = (idx+1,res,src)
        | testElem > prevElem && testElem > nextElem = (idx+1,res ++ [src !! idx],src)
        | otherwise                                  = (idx+1,res,src)
          where
            testElem = src !! idx
            prevElem = src !! (idx-1)
            nextElem = src !! (idx+1)

