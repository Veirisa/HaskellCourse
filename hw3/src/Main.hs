{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}

module Main where

import           Control.Monad              (liftM2)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask, local)
import           Control.Monad.State        (MonadState, get, modify, put)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
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

-------------------------- DATA AND TYPES --------------------------

data Expr = Lit Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let String Expr Expr
    deriving Show

data ExprError = DivError String | NotEvalError String String

instance Show ExprError where
    show :: ExprError -> String
    show (DivError actName) =
          "The expression for " ++ actName ++ " contains division by 0"
    show (NotEvalError varName actName) =
        "The expression for " ++ actName ++
        " can't be fully evaluated because variable \"" ++  varName ++ "\" doesn't exist"

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
    , MonadState (M.Map String Int, Int) m
    )

type InteprConstraint m =
    ( InteprConstraintWithoutIO m
    , MonadIO m
    )

----------------------------- PARSERS ------------------------------

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

---------------------------- FUNCTIONS -----------------------------

eval :: ( MonadError InterprError m
        , MonadReader (M.Map String Int, Int, String) m
        )
    => Expr -> m Int
eval (Lit val) = return val
eval (Var name) = do
    (m, num, act) <- ask
    case M.lookup name m of
        Nothing  -> throwError $ InterprExprError (NotEvalError name act) num
        Just val -> return val
eval (Add l r) = liftM2 (+) (eval l) (eval r)
eval (Sub l r) = liftM2 (-) (eval l) (eval r)
eval (Mul l r) = liftM2 (*) (eval l) (eval r)
eval (Div l r) = do
    rCalced <- eval r
    if rCalced == 0
    then do
        (m, num, act) <- ask
        throwError $ InterprExprError (DivError act) num
    else do
        lCalced <- eval l
        return $ div lCalced rCalced
eval (Let v eqExpr inExpr) = do
    eqExprCalced <- eval eqExpr
    let changeMap = \(curM, curNum, curAct) -> (M.insert v eqExprCalced curM, curNum, curAct)
    local changeMap (eval inExpr)

varActionWithExcept :: InteprConstraintWithoutIO m
    => String -> Int -> Bool -> (String -> ActionError) -> m ()
varActionWithExcept name val mustBeMember constrError = do
    (m, num) <- get
    if not (M.member name m == mustBeMember)
    then throwError $ InterprActionError (constrError name) num
    else modify $ \(curM, curNum) -> (M.insert name val curM, curNum + 1)

creature :: InteprConstraintWithoutIO m
    => String -> Int -> m ()
creature name val = varActionWithExcept name val False CreatureError

assignment :: InteprConstraintWithoutIO m
    => String -> Int -> m ()
assignment name val = varActionWithExcept name val True AssignmentError

readVar :: InteprConstraint m
    => String -> m ()
readVar name= do
    valStr <- liftIO $ getLine
    varActionWithExcept name (read valStr) True ReadError

writeExpr :: InteprConstraint m
    => Int  -> m ()
writeExpr val = do
    liftIO $ putStrLn (show val)
    modify $ \(curM, curNum) -> (curM, curNum + 1)

-------------------------- INTERPRITATION --------------------------

getActionLength :: Action -> Int
getActionLength (For _ _ _ actions) = 2 + getActionsLength actions
getActionLength _                   = 1

getActionsLength :: [Action] -> Int
getActionsLength actions = sum $ map getActionLength actions

interpritationFor :: InteprConstraint m
    => [Action] -> String -> Int -> m ()
interpritationFor actions name toVal = do
    (m, num) <- get
    case M.lookup name m of
        Nothing -> throwError $ InterprActionError (AssignmentError name) num
        Just curVal ->
            if curVal >= toVal
            then
                modify $ \(curM, curNum) -> (curM, curNum + getActionsLength actions + 1)
            else do
                fullInterpritation actions
                interpritationWithOneExpr (Add (Var name) (Lit 1)) (assignment name)  ("assignment " ++ name ++ "\"")
                modify $ \(curM, curNum) -> (curM, num)
                interpritationFor actions name toVal

interpritationWithOneExpr :: InteprConstraint m
    => Expr -> (Int -> m ()) -> String -> m ()
interpritationWithOneExpr expr varAction actName = do
    (m, num) <- get
    val <- runReaderT (eval expr) (m, num, actName)
    varAction val

interpritation :: InteprConstraint m
    => Action -> m ()
interpritation (Creature name expr) =
    interpritationWithOneExpr expr (creature name) ("creature \"" ++ name ++ "\"")
interpritation (Assignment name expr) =
    interpritationWithOneExpr expr (assignment name) ("assignment \"" ++ name ++ "\"")
interpritation (Write expr) =
    interpritationWithOneExpr expr writeExpr "writing"
interpritation (Read name) = readVar name
interpritation (For name fromExpr toExpr actions) = do
    (m, num) <- get
    let env = (m, num, "assignment \"" ++ name ++ "\"")
    fromVal <- runReaderT (eval fromExpr) env
    toVal <- runReaderT (eval toExpr) env
    assignment name fromVal
    interpritationFor actions name toVal

fullInterpritation :: InteprConstraint m
    => [Action] -> m ()
fullInterpritation actions = mapM_ interpritation actions

----------------------------- STARTING -----------------------------

main :: IO ()
main = do
    path <- getLine
    code <- readFile path
    case runParser parserProgram "" code of
        Left parseErr -> putStr $ parseErrorPretty parseErr
        Right prog -> do
            eitherInterpr <- runStateT (runExceptT (fullInterpritation prog)) (M.fromList [], 1)
            case eitherInterpr of
                (Left interprErr, newSt) -> putStrLn $ show interprErr
                (Right (), newSt)        -> return ()
