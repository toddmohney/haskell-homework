module Calc where
  import ExprT

  eval :: ExprT -> Integer
  eval (Lit int) = int
  eval (Add exp1 exp2) = (eval exp1) + (eval exp2)
  eval (Mul exp1 exp2) = (eval exp1) * (eval exp2)
