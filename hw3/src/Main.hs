{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Main where

import           Control.Monad              (liftM2)
import           Control.Monad.Reader       (Reader, ask, local, runReader)
import           Data.Char                  (isDigit, isLetter, isLower)
import           Data.Either                (fromRight, isLeft, isRight)
import qualified Data.Map                   as M (Map, delete, fromList, insert,
                                                  member, (!))

import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except (throwE)
-- import           Control.Monad.Trans.Reader
-- import           Control.Monad.Trans.State

------------------------------ TASK 1 ------------------------------

data Expr = Lit Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let String Expr Expr
    deriving Show

data ExprError = DivError | NotEvalError String
    deriving Eq

instance Show ExprError where
    show :: ExprError -> String
    show DivError = "The expression contains division by 0"
    show (NotEvalError name)
        = "The expression can't be fully evaluated because variable \""
        ++ name ++ "\" doesn't exist"

eval :: Expr -> ExceptT ExprError (Reader (M.Map String Int)) Int
eval (Lit x) = return x
eval (Var v) = do
  m <- lift ask
  if not (M.member v m)
  then throwE (NotEvalError v)
  else return (m M.! v)
eval (Add l r) = liftM2 (+) (eval l) (eval r)
eval (Sub l r) = liftM2 (-) (eval l) (eval r)
eval (Mul l r) = liftM2 (*) (eval l) (eval r)
eval (Div l r) = do
    rCalced <- eval r
    if rCalced == 0
    then throwE DivError
    else do
        lCalced <- eval l
        return $ div lCalced rCalced
eval (Let v eqExpr inExpr) = do
    eqExprCalced <- eval eqExpr
    local (M.insert v eqExprCalced) (eval inExpr)

------------------------------ TASK 2 ------------------------------

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

rws :: [String]
rws = ["let", "in", "mut"]

identifier :: Parser String
identifier = (lexeme . try) (correctName >>= check)
  where
    correctName = (:) <$> letterChar <*> many alphaNumChar
    check x =
       if x `elem` rws
       then fail $ "keyword \"" ++ show x ++ "\" can't be a variable name"
       else return x

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

parserLet :: Parser Expr
parserLet = do
  rword "let"
  var <- identifier
  symbol "="
  eqExpr <- parserExpr
  rword "in"
  inExpr <- parserExpr
  return (Let var eqExpr inExpr)

parserExpr :: Parser Expr
parserExpr = makeExprParser exprTerm exprOperators

exprOperators :: [[Operator Parser Expr]]
exprOperators =
  [ [ InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/") ]
  , [ InfixL (Add <$ symbol "+")
    , InfixL (Sub <$ symbol "-") ] ]

exprTerm :: Parser Expr
exprTerm =
  parens (parserExpr <|> parserLet)
  <|> Var <$> identifier
  <|> Lit <$> integer

----------------------------- TASK 3-7 -----------------------------

------- Structures

data Action = Creature String Expr
            | Assignment String Expr
            | Read String
            | Write Expr
    deriving Show

data ActionError = CreatureError String
                 | AssignmentError String
                 | ReadError String

instance Show ActionError where
    show :: ActionError -> String
    show (CreatureError name) =
        "The variable \"" ++ name ++ "\" can't be created because it already exists"
    show (AssignmentError name) =
        "The variable \"" ++ name ++ "\" can't be changed because it doesn't exist"
    show (ReadError name) =
        "The variable \"" ++ name ++ "\" can't be readed because it doesn't exist"

data InterprError = InterprExprError ExprError Int
                  | InterprActionError ActionError Int

instance Show InterprError where
    show :: InterprError -> String
    show (InterprExprError err num) =
        show num ++ ": " ++ show err
    show (InterprActionError err num) =
        show num ++ ": " ++ show err

------- Functions

creature :: String -> Int -> ExceptT ActionError (State (M.Map String Int)) ()
creature name val = do
  m <- lift get
  if not (M.member name m)
  then do
    lift $ modify (M.insert name val)
    return ()
  else throwE (CreatureError name)


assignment :: String -> Int -> ExceptT ActionError (State (M.Map String Int)) ()
assignment name val = do
  m <- lift get
  if M.member name m
  then do
      lift $ modify (M.insert name val)
      return ()
  else throwE (AssignmentError name)

writeExpr :: Int -> ExceptT ActionError (StateT (M.Map String Int) IO) ()
writeExpr val = do
  lift $ lift $ putStrLn (show val)

readVar :: String -> ExceptT ActionError (StateT (M.Map String Int) IO) ()
readVar name = do
  m <- lift get
  valStr <- lift $ lift $ getLine
  if M.member name m
  then do
      lift $ modify (M.insert name (read valStr))
      return ()
  else throwE (ReadError name)

------- Parsers

parserCreature :: Parser Action
parserCreature = do
  rword "mut"
  var <- identifier
  symbol "="
  expr <- parserExpr
  return (Creature var expr)

parserAssignment :: Parser Action
parserAssignment = do
  var <- identifier
  symbol "="
  expr <- parserExpr
  return (Assignment var expr)

parserRead :: Parser Action
parserRead = do
  symbol ">"
  var <- identifier
  return (Read var)

parserWrite :: Parser Action
parserWrite = do
  symbol "<"
  expr <- parserExpr
  return (Write expr)

parserAction :: Parser Action
parserAction = parserCreature <|> parserAssignment <|> parserRead <|> parserWrite

------- Interpritation

interpritation :: [Action] -> State (M.Map String Int) ()
interpritation = undefined

------------------------------ TASK 8* -----------------------------

------------------------------ TASK 9* -----------------------------

------------------------------ TASK 10 -----------------------------

parserProgram :: Parser [Action]
parserProgram = many parserAction

main :: IO ()
main = putStrLn "hw3"
