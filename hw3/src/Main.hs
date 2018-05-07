{-# LANGUAGE InstanceSigs #-}

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

import           Control.Monad.State.Lazy   (State, state)

------------------------------ TASK 1 ------------------------------

data Expr = Lit Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let String Expr Expr
    deriving Show

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

type ReaderForEval = Reader (M.Map String Int) (Either ExprError Int)

liftM2Expr :: (Int -> Int -> Int) -> Expr -> Expr -> ReaderForEval
liftM2Expr op l r = do
  lCalced <- eval l
  rCalced <- eval r
  return $ liftM2 op lCalced rCalced

eval :: Expr -> ReaderForEval
eval (Lit x) = return $ Right x
eval (Var v) = do
  m <- ask
  if not (isCorrectName v)
  then return $ Left (NameError v)
  else
      if not (M.member v m)
      then return $ Left NotEvalError
      else return $ Right (m M.! v)
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
        else local (M.insert v (fromRight 0 eqExprCalced)) (eval inExpr)

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

name :: Parser String
name = (lexeme . try) (correctName >>= check)
  where
    correctName = (:) <$> letterChar <*> many alphaNumChar
    check x =
       if x `elem` rws
       then fail $ "keyword " ++ show x ++ " can't be a variable name"
       else return x

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

parserLet :: Parser Expr
parserLet = do
  rword "let"
  var <- name
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
  <|> Var <$> name
  <|> Lit <$> integer

---------------------------- STRUCTURES ----------------------------

data Action = Creature String Expr
            | Assignment String Expr
            | Read String
            | Write Expr
    deriving Show

data VerdictAction = Suсcess | Fail ActionError

data ActionError = CreatureError String
                 | AssigmentError String
                 | ReadError String

instance Show ActionError where
    show :: ActionError -> String
    show (CreatureError name) =
        "The variable " ++ name ++ " can't be created because it already exists"
    show (AssigmentError name) =
        "The variable " ++ name ++ " can't be changed because it doesn't exist"
    show (ReadError name) =
        "The variable " ++ name ++ " can't be readed because it doesn't exist"

data InterprError = InterprExprError ExprError Int
                  | InterprActionError ActionError Int

instance Show InterprError where
    show :: InterprError -> String
    show (InterprExprError err num) =
        show num ++ ": " ++ show err
    show (InterprActionError err num) =
        show num ++ ": " ++ show err

------------------------------ TASK 3 ------------------------------

creature :: String -> Int -> State (M.Map String Int) VerdictAction
creature name val = eqAction name val False (CreatureError name)

assignment :: String -> Int -> State (M.Map String Int) VerdictAction
assignment name val = eqAction name val True (AssigmentError name)

eqAction :: String -> Int -> Bool -> ActionError
             -> State (M.Map String Int) VerdictAction
eqAction name val mustBeMember err = state $ \m ->
    if M.member name m == mustBeMember
    then (Suсcess, M.insert name val m)
    else (Fail err, m)

------------------------------ TASK 4 ------------------------------

parserAction :: Parser Action
parserAction = parserCreature <|> parserAssigment <|> parserRead <|> parserWrite

parserCreature :: Parser Action
parserCreature = do
  rword "mut"
  var <- name
  symbol "="
  expr <- parserExpr
  return (Creature var expr)

parserAssigment :: Parser Action
parserAssigment = do
  var <- name
  symbol "="
  expr <- parserExpr
  return (Assignment var expr)

parserRead :: Parser Action
parserRead = do
  symbol ">"
  var <- name
  return (Read var)

parserWrite :: Parser Action
parserWrite = do
  symbol "<"
  expr <- parserExpr
  return (Write expr)

------------------------------ TASK 5 ------------------------------

interpritation :: [Action] -> State (M.Map String Int) ()
interpritation = undefined

------------------------------ TASK 6 ------------------------------

------------------------------ TASK 7 ------------------------------

------------------------------ TASK 8* -----------------------------

------------------------------ TASK 9* -----------------------------

------------------------------ TASK 10 -----------------------------

------------------------------- MAIN -------------------------------

main :: IO ()
main = putStrLn "hw3"
