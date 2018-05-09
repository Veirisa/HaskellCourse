{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}

module Main where

import           Control.Monad              (liftM2)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask, local, runReader)
import           Control.Monad.State        (MonadState, get, modify, put)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.State  (StateT, runStateT)

import qualified Data.Map                   as M (Map, delete, fromList, insert,
                                                  lookup, member, (!))
import           Data.Void                  (Void)

import           Text.Megaparsec            (Parsec, between, empty, many,
                                             notFollowedBy, runParser, try,
                                             (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, letterChar, space1,
                                             string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, space,
                                                  symbol)
import           Text.Megaparsec.Error      (parseErrorPretty)
import           Text.Megaparsec.Expr       (Operator (InfixL), makeExprParser)

-- parseErrorTextPretty

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

instance Show ExprError where
    show :: ExprError -> String
    show DivError = "The expression contains division by 0"
    show (NotEvalError name)
        = "The expression can't be fully evaluated because variable \""
          ++ name ++ "\" doesn't exist"

eval :: ( MonadError ExprError m
        , MonadReader (M.Map String Int) m
        )
    => Expr -> m Int
eval (Lit val) = return val
eval (Var name) = do
    m <- ask
    case M.lookup name m of
        Nothing  -> throwError $ NotEvalError name
        Just val -> return val
eval (Add l r) = liftM2 (+) (eval l) (eval r)
eval (Sub l r) = liftM2 (-) (eval l) (eval r)
eval (Mul l r) = liftM2 (*) (eval l) (eval r)
eval (Div l r) = do
    rCalced <- eval r
    if rCalced == 0
    then throwError DivError
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
rws = ["let", "in", "mut", "for", "from", "to"]

identifier :: Parser String
identifier = (lexeme . try) (correctName >>= check)
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
    varName <- identifier
    symbol "="
    eqExpr <- parserExpr
    rword "in"
    inExpr <- parserExpr
    return $ Let varName eqExpr inExpr

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

----------------------------- TASKS 3-8 ----------------------------

------- Structures

data Action = Creature String Expr
            | Assignment String Expr
            | Read String
            | Write Expr
            | For String Expr Expr [Action]
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
        "(" ++ show num ++ "): " ++ show err
    show (InterprActionError err num) =
        "(" ++ show num ++ "): " ++ show err

type InteprConstraintWithoutIO m =
    ( MonadError InterprError m
    , MonadState (M.Map String Int) m
    )

type InteprConstraint m =
    ( InteprConstraintWithoutIO m
    , MonadIO m
    )

------- Functions

varActionWithExcept :: InteprConstraintWithoutIO m
    => String -> Int -> Bool -> (String -> ActionError) -> Int -> m ()
varActionWithExcept name val mustBeMember constrError num = do
    m <- get
    if not (M.member name m == mustBeMember)
    then throwError $  InterprActionError (constrError name) num
    else modify (M.insert name val)

creature :: InteprConstraintWithoutIO m
    => String -> Int -> Int -> m ()
creature name val num = varActionWithExcept name val False CreatureError num

assignment :: InteprConstraintWithoutIO m
    => String -> Int -> Int -> m ()
assignment name val num = varActionWithExcept name val True AssignmentError num

readVar :: InteprConstraint m
    => String -> Int -> m ()
readVar name num = do
    valStr <- liftIO $ getLine
    varActionWithExcept name (read valStr) True ReadError num

writeExpr :: InteprConstraint m
    => Int -> Int -> m ()
writeExpr val num = liftIO $ putStrLn (show val)

------- Parsers

parserCreature :: Parser Action
parserCreature = do
    rword "mut"
    varName <- identifier
    symbol "="
    expr <- parserExpr
    return $ Creature varName expr

parserAssignment :: Parser Action
parserAssignment = do
    varName <- identifier
    symbol "="
    expr <- parserExpr
    return $ Assignment varName expr

parserRead :: Parser Action
parserRead = do
    symbol ">"
    varName <- identifier
    return $ Read varName

parserWrite :: Parser Action
parserWrite = do
    symbol "<"
    expr <- parserExpr
    return $ Write expr

parserFor :: Parser Action
parserFor = do
    rword "for"
    symbol "("
    varName <- identifier
    rword "from"
    fromExpr <- parserExpr
    rword "to"
    toExpr <- parserExpr
    symbol ")"
    symbol "{"
    actions <- parserProgram
    symbol "}"
    return $ For varName fromExpr toExpr actions

parserAction :: Parser Action
parserAction =
    parserCreature
    <|> parserAssignment
    <|> parserRead
    <|> parserWrite
    <|> parserFor

parserProgram :: Parser [Action]
parserProgram = many parserAction

------- Interpritation

interpritationFor :: InteprConstraint m
    => [Action] -> String -> Int -> Int -> m ()
interpritationFor actions name toVal num = do
    m <- get
    case M.lookup name m of
        Nothing -> throwError $ InterprActionError (AssignmentError name) num
        Just curVal ->
            if curVal >= toVal
            then return ()
            else do
                interpritation actions (num + 1)
                interpritationWithOneCalcExpr [] (Add (Var name) (Lit 1)) (assignment name) num
                interpritationFor actions name toVal num

interpritationWithOneCalcExpr :: InteprConstraint m
    => [Action] -> Expr -> (Int -> Int -> m ()) -> Int -> m ()
interpritationWithOneCalcExpr nextActions expr varAction num = do
    st <- get
    case runReader (runExceptT (eval expr)) st of
        Left err -> throwError $ InterprExprError err num
        Right val -> do
            varAction val num
            interpritation nextActions (num + 1)

interpritation :: InteprConstraint m
    => [Action] -> Int -> m ()
interpritation [] _ = return ()
interpritation ((Creature name expr) : nextActions) num =
    interpritationWithOneCalcExpr nextActions expr (creature name) num
interpritation ((Assignment name expr) : nextActions) num =
    interpritationWithOneCalcExpr nextActions expr (assignment name) num
interpritation ((Write expr) : nextActions) num =
    interpritationWithOneCalcExpr nextActions expr writeExpr num
interpritation ((Read name) : nextActions) num = do
    readVar name num
    interpritation nextActions (num + 1)
interpritation ((For name fromExpr toExpr actions) : nextActions) num = do
    st <- get
    case runReader (runExceptT (eval fromExpr)) st of
        Left err -> throwError $ InterprExprError err num
        Right fromVal ->
            case runReader (runExceptT (eval toExpr)) st of
                Left err' -> throwError $ InterprExprError err' num
                Right toVal -> do
                    assignment name fromVal num
                    interpritationFor actions name toVal num
                    interpritation nextActions (num + length actions + 2)

------------------------------ TASK 10 -----------------------------

main :: IO ()
main = do
    path <- getLine
    code <- readFile path
    case runParser parserProgram "" code of
        Left parseErr -> putStr $ parseErrorPretty parseErr
        Right prog -> do
            eitherInterpr <- runStateT (runExceptT (interpritation prog 1)) (M.fromList [])
            case eitherInterpr of
                (Left interprErr, newSt) -> putStrLn $ show interprErr
                (Right (), newSt)        -> return ()
