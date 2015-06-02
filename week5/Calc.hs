{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where
  import ExprT
  import Parser
  import qualified StackVM

  newtype MinMax = MinMax Integer deriving (Eq, Show)
  newtype Mod7   = Mod7 Integer deriving (Eq, Show)

  class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

  instance Ord MinMax where
    min (MinMax a) (MinMax b) = MinMax $ min a b
    max (MinMax a) (MinMax b) = MinMax $ max a b
    compare (MinMax a) (MinMax b) = compare a b

  instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

  instance Expr Integer where
    lit = id
    add a b = a + b
    mul a b = a * b

  instance Expr Bool where
    lit a = a > 0
    add a b = a || b
    mul a b = a && b

  instance Expr MinMax where
    lit = MinMax
    add a b = min a b
    mul a b = max a b

  instance Expr Mod7 where
    lit a = Mod7 $ a `mod` 7 
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

  instance Expr StackVM.Program where
    lit a = [StackVM.PushI a]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

  eval :: ExprT -> Integer
  eval (Lit int) = int
  eval (Add exp1 exp2) = (eval exp1) + (eval exp2)
  eval (Mul exp1 exp2) = (eval exp1) * (eval exp2)

  evalStr :: String -> Maybe Integer
  evalStr str = fmap eval (parseExp Lit Add Mul str)

  reify :: ExprT -> ExprT
  reify = id

  compile :: String -> Maybe StackVM.Program
  compile = parseExp lit add mul

  testExp :: Expr a => Maybe a
  testExp = parseExp lit add mul "(3 * -4) + 5"

  testInteger = testExp :: Maybe Integer
  testBool = testExp :: Maybe Bool
  testMM = testExp :: Maybe MinMax
  testSat = testExp :: Maybe Mod7
