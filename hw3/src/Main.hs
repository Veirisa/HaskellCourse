{-# LANGUAGE InstanceSigs #-}

module Main where

import           Control.Monad        (liftM2)
import           Control.Monad.Reader (Reader, ask, local, runReader)
import           Data.Char            (isDigit, isLetter, isLower)
import           Data.Either          (fromRight, isLeft, isRight)
import qualified Data.Map             as HM (Map, delete, fromList, insert,
                                             member, (!))

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

type ReaderForEval = Reader (HM.Map String Int) (Either ExprError Int)

liftM2Expr :: (Int -> Int -> Int) -> Expr -> Expr -> ReaderForEval
liftM2Expr op l r = do
  lCalced <- eval l
  rCalced <- eval r
  return $ liftM2 op lCalced rCalced

eval :: Expr -> ReaderForEval
eval (Lit x) = return $ Right x
eval (Var v) = do
  hm <- ask
  if not (isCorrectName v)
  then return $ Left (NameError v)
  else
      if not (HM.member v hm)
      then return $ Left NotEvalError
      else return $ Right (hm HM.! v)
eval (Add l r) = liftM2Expr (+) l r
eval (Sub l r) = liftM2Expr (-) l r
eval (Mul l r) = liftM2Expr (*) l r
eval (Div l r) = do
    rCalced <- eval r
    if fromRight 1 rCalced == 0
    then return $ Left DivError
    else do
        lCalced <- eval l
        return $ liftM2 div lCalced rCalced
eval (Let v eqExpr inExpr) = do
    if not (isCorrectName v)
    then return $ Left (NameError v)
    else do
        eqExprCalced <- eval eqExpr
        if isLeft eqExprCalced
        then return $ eqExprCalced
        else local (HM.insert v (fromRight 0 eqExprCalced)) (eval inExpr)

main :: IO ()
main = putStrLn "hw3"
