{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where
  import Data.Char(toUpper)
  import Data.Monoid

  newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

  instance Monoid Score where
    mempty = Score 0
    mappend = (+)

  score :: Char -> Score
  score = undefined

  scoreString :: String -> Score
  scoreString word = Score $ (foldr (+) 0) $ map scoreLetter $ map toUpper word 

  scoreLetter :: Char -> Int
  scoreLetter l
      | l `elem` "EAIONRTLSU" = 1
      | l `elem` "DG"         = 2
      | l `elem` "BCMP"       = 3
      | l `elem` "FHVWY"      = 4
      | l `elem` "K"          = 5
      | l `elem` "JX"         = 8
      | l `elem` "QZ"         = 10
      | otherwise             = 0

