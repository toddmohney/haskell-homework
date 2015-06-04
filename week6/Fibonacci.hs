module Fibonacci where

  fib :: Integer -> Integer
  fib n
    | n < 1     = 0
    | n == 1    = 1
    | otherwise = (fib (n-1)) + (fib (n-2))

  fibs1:: [Integer]
  fibs1 = map fib [0..]

  fibs2 :: [Integer]
  fibs2 = map fibs2' [0..]
    where 
      fibs2' n 
        | n < 1     = 0
        | n == 1    = 1
        | otherwise = (fibs2 !! (n-2)) + (fibs2 !! (n-1))

  data Stream a = Stream a (Stream a)

  instance Show a => Show (Stream a) where
    show stream = unwords (map show $ take 20 $ streamToList stream)

  streamToList :: (Stream a) -> [a]
  streamToList (Stream a stream) = a : streamToList stream

  streamRepeat :: a -> Stream a
  streamRepeat a = Stream a (streamRepeat a)

