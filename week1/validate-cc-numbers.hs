import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits i 
  | i <= 0 = []
  | otherwise = map (toInteger . digitToInt) (show i)


toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) oneTwo 
  where oneTwo = 1 : 2 : oneTwo


sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)


validate :: Integer -> Bool
validate i = (sumDigits . doubleEveryOther . toDigitsRev) i `mod` 10 == 0
