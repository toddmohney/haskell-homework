import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits i 
  | i <= 0 = []
  | otherwise = map (toInteger . digitToInt) (show i)


toDigitsRev :: Integer -> [Integer]
toDigitsRev i = reverse $ toDigits i

