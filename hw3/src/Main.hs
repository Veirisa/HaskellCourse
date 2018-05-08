{-# LANGUAGE InstanceSigs #-}

module Main where

import           Control.Monad              (liftM2)
import           Control.Monad.Reader       (Reader, ask, local, runReader)
import           Data.Char                  (isDigit, isLetter, isLower)
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
rws = ["let", "in", "mut", "for"]

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
  varName <- identifier
  symbol "="
  eqExpr <- parserExpr
  rword "in"
  inExpr <- parserExpr
  return (Let varName eqExpr inExpr)

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
            | For Int [Action]
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

varActionWithExcept :: String -> Int -> Bool -> (String -> ActionError) -> Int
                       -> ExceptT InterprError (StateT (M.Map String Int) IO) ()
varActionWithExcept name val mustBeMember constrError num = do
    m <- lift get
    if M.member name m == mustBeMember
    then do
        lift $ modify (M.insert name val)
        return ()
    else throwE $ InterprActionError (constrError name) num

creature :: String -> Int -> Int -> ExceptT InterprError (StateT (M.Map String Int) IO) ()
creature name val num = varActionWithExcept name val False CreatureError num

assignment :: String -> Int -> Int -> ExceptT InterprError (StateT (M.Map String Int) IO) ()
assignment name val num = varActionWithExcept name val True AssignmentError num

readVar :: String -> Int -> ExceptT InterprError (StateT (M.Map String Int) IO) ()
readVar name num = do
    valStr <- lift $ lift $ getLine
    varActionWithExcept name (read valStr) True ReadError num

writeExpr :: Int -> Int -> ExceptT InterprError (StateT (M.Map String Int) IO) ()
writeExpr val num = lift $ lift $ putStrLn (show val)

------- Parsers

parserCreature :: Parser Action
parserCreature = do
  rword "mut"
  varName <- identifier
  symbol "="
  expr <- parserExpr
  return (Creature varName expr)

parserAssignment :: Parser Action
parserAssignment = do
  varName <- identifier
  symbol "="
  expr <- parserExpr
  return (Assignment varName expr)

parserRead :: Parser Action
parserRead = do
  symbol ">"
  varName <- identifier
  return (Read varName)

parserWrite :: Parser Action
parserWrite = do
  symbol "<"
  expr <- parserExpr
  return (Write expr)

parserFor :: Parser Action
parserFor = do
  rword "for"
  symbol "("
  counterFrom <- integer
  symbol ","
  counterTo <- integer
  symbol ")"
  symbol "{"
  actions <- parserProgram
  symbol "}"
  return (For (max 0 (counterTo - counterFrom)) actions)

parserAction :: Parser Action
parserAction =
    parserCreature
    <|> parserAssignment
    <|> parserRead
    <|> parserWrite
    <|> parserFor

------- Interpritation

interpritationFor :: Int -> [Action] -> Int -> ExceptT InterprError (StateT (M.Map String Int) IO) ()
interpritationFor 0 _ _ = return ()
interpritationFor rep actions num = do
    interpritation actions (num + 1)
    interpritationFor (rep - 1) actions num

interpritationWithCalcExpr :: [Action] -> Expr -> (Int -> Int -> ExceptT InterprError (StateT (M.Map String Int) IO) ())
                              -> Int -> ExceptT InterprError (StateT (M.Map String Int) IO) ()
interpritationWithCalcExpr nextActs expr varAction num = do
    st <- lift $ get
    case runReader (runExceptT (eval expr)) st of
        Left err -> throwE (InterprExprError err num)
        Right val -> do
            varAction val num
            interpritation nextActs (num + 1)

interpritation :: [Action] -> Int -> ExceptT InterprError (StateT (M.Map String Int) IO) ()
interpritation [] _ = return ()
interpritation ((Creature name expr) : nextActs) num =
    interpritationWithCalcExpr nextActs expr (creature name) num
interpritation ((Assignment name expr) : nextActs) num =
    interpritationWithCalcExpr nextActs expr (assignment name) num
interpritation ((Write expr) : nextActs) num =
    interpritationWithCalcExpr nextActs expr writeExpr num
interpritation ((Read name) : nextActs) num = do
    readVar name num
    interpritation nextActs (num + 1)
interpritation ((For rep actions) : nextActs) num = do
    interpritationFor rep actions num
    interpritation nextActs (num + length actions + 2)

------------------------------ TASK 8* -----------------------------

------------------------------ TASK 9* -----------------------------

------------------------------ TASK 10 -----------------------------

parserProgram :: Parser [Action]
parserProgram = many parserAction

main :: IO ()
main = do
    path <- getLine
    code <- readFile path
    case runParser parserProgram "" code of
        Left err -> putStrLn $ show err
        Right pr -> do
            eitherInterpr <- runStateT (runExceptT (interpritation pr 1)) (M.fromList [])
            case eitherInterpr of
              (Left err', newSt) -> putStrLn $ show err'
              (Right (), newSt)  -> return ()
