{-# LANGUAGE InstanceSigs #-}

module Main where

import           Control.Monad (liftM2)
import           Data.Char     (isDigit, isLetter, isLower)
import           Data.Either   (fromRight, isLeft, isRight)
import qualified Data.Map      as HM (Map, delete, fromList, insert, member,
                                      (!))

------------------------------ TASK 1 ------------------------------

data Expr = Lit Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let String Expr Expr

data ExprError = DivError | NotEvalError | NameError String
    deriving Eq

instance Show ExprError where
    show :: ExprError -> String
    show DivError         = "The expression contains division by 0"
    show NotEvalError     = "The expression can't be fully evaluated"
    show (NameError name) = "Name \"" ++ name ++ "\" is incorrect"

isCorrectName :: String -> Bool
isCorrectName (x : xs) = isLower x && (all (\c -> isDigit c || isLetter c) xs)
isCorrectName _        = False

eval :: Expr -> HM.Map String Int -> Either ExprError Int
eval (Lit x) hm = return x
eval (Var v) hm =
  if not (isCorrectName v)
  then Left (NameError v)
  else
      if HM.member v hm
      then return (hm HM.! v)
      else Left NotEvalError
eval (Add l r) hm = liftM2 (+) (eval l hm) (eval r hm)
eval (Sub l r) hm = liftM2 (-) (eval l hm) (eval r hm)
eval (Mul l r) hm = liftM2 (*) (eval l hm) (eval r hm)
eval (Div l r) hm =
  let
    rCalced = eval r hm
    isNewError = fromRight 1 rCalced == 0
  in
    liftM2 div (eval l hm) (if isNewError then Left DivError else rCalced)
eval (Let v eqExpr inExpr) hm =
  let
    eqExprCalced =
      if isCorrectName v
      then eval eqExpr hm
      else Left (NameError v)
    newHM =
      if isRight eqExprCalced
      then HM.insert v (fromRight 0 eqExprCalced) hm
      else hm
  in
    if isLeft eqExprCalced then eqExprCalced else eval inExpr newHM

main :: IO ()
main = putStrLn "hw3"
