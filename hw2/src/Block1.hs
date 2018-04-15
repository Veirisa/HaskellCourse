{-# LANGUAGE InstanceSigs #-}

module Block1 where

import           Control.Monad (liftM2)
import           Data.Either   (fromRight)

------------------------------ TASK 1 ------------------------------

data Expr = Const Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr

data ArithmeticError = DivError | PowError
    deriving Eq

instance Show ArithmeticError where
    show :: ArithmeticError -> String
    show DivError = "The expression contains division by 0"
    show PowError = "The expression contains a negative exponentiation"

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = return x
eval (Add l r) = liftM2 (+) (eval l) (eval r)
eval (Sub l r) = liftM2 (-) (eval l) (eval r)
eval (Mul l r) = liftM2 (*) (eval l) (eval r)
eval (Div l r) =
  let
    rCalced = eval r
    isNewError = fromRight 1 rCalced == 0
  in
    liftM2 div (eval l) (if isNewError then Left DivError else rCalced)
eval (Pow l r) =
  let
    rCalced = eval r
    isNewError = fromRight 1 rCalced < 0
  in
    liftM2 (^) (eval l) (if isNewError then Left PowError else rCalced)

------------------------------ TASK 2 ------------------------------

bin n = foldr ($) [[]] (replicate n (>>= \s -> [0 : s, 1 : s]))
