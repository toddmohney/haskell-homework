module CreditCardNumbers where
  import Data.Char (digitToInt)

  toDigits :: Integer -> [Integer]
  toDigits i
    | i < 0     = []
    | i < 10    = [i]
    | otherwise = map digitToInteger $ show i
      where 
        digitToInteger :: Char -> Integer
        digitToInteger = toInteger . digitToInt

  toDigitsRev :: Integer -> [Integer]
  toDigitsRev = reverse . toDigits

  doubleEveryOther :: [Integer] -> [Integer]
  doubleEveryOther = reverse . zipWith (*) oneTwos . reverse
    where 
      oneTwos :: [Integer]
      oneTwos = cycle [1,2]

  sumDigits :: [Integer] -> Integer
  sumDigits = sum . concatMap toDigits

  validate :: Integer -> Bool
  validate ccNum = buildChecksum ccNum `mod` 10 == 0
    where
      buildChecksum :: Integer -> Integer
      buildChecksum = sumDigits . doubleEveryOther . toDigits

