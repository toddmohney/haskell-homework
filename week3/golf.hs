{-# OPTIONS_GHC -Wall #-}
module Golf where
  import Data.List (maximumBy)

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

  type NumberCounter = (Int,Int) -- (number, occurencesOfTheNumber)

  histogram :: [Int] -> String
  histogram = histogram' . foldr buildAssociationList []
    where 
      buildAssociationList :: Int -> [NumberCounter] -> [NumberCounter]
      buildAssociationList num list = addToAssociationList num (lookup num list) list

      addToAssociationList :: Int -> Maybe Int -> [NumberCounter] -> [NumberCounter]
      addToAssociationList num (Just count) list = addToAL list (num,count+1)
      addToAssociationList num Nothing list      = addToAL list (num,1)

      histogram' :: [NumberCounter] -> String
      histogram' xs = concatMap (drawLine xs) (reverse [1..(maxOccurences xs)]) ++ drawSeparator ++ "\n" ++ drawLegend ++ "\n"

      drawLine :: [NumberCounter] -> Int -> String
      drawLine xs num = concatMap (drawSymbol xs num) numberRange ++ "\n"
        where
          drawSymbol :: [NumberCounter] -> Int -> Int -> String
          drawSymbol xs' occ num' = determineSymbol (lookup num' xs') occ
            where 
              determineSymbol :: Maybe Int -> Int -> String
              determineSymbol Nothing _ = " "
              determineSymbol (Just n) occ'
                | n >= occ' = "*"
                | otherwise = " "

      drawSeparator :: String
      drawSeparator = concat $ replicate (length numberRange) "="

      drawLegend :: String
      drawLegend = concatMap show numberRange

      numberRange :: [Int]
      numberRange = [0..9]

  maxOccurences :: [NumberCounter] -> Int
  maxOccurences xs = getValue $ maximumBy (\(_,v) (_,v') -> compare v v') xs

  getValue :: NumberCounter -> Int
  getValue (_,v) = v

  addToAL :: (Eq a) => [(a,b)] -> (a,b) -> [(a,b)]
  addToAL [] (k',v') = [(k',v')]
  addToAL ((k,v):xs) (k',v')
    | k == k'   = addToAL xs (k',v')
    | otherwise = (k,v) : addToAL xs (k',v')

