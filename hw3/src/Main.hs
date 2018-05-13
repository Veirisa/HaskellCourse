{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (liftM2)
import           Control.Monad.Cont         (MonadCont, callCC, runCont, when)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask, local)
import           Control.Monad.State        (MonadState, get, modify)
import           Control.Monad.Trans.Cont   (runContT)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.State  (runStateT)

import           Data.ByteString            (cons)
import qualified Data.ByteString.Char8      as C (ByteString, append, pack,
                                                  putStrLn, readFile)
import           Data.Char                  (chr, isAlphaNum)
import qualified Data.Map                   as M (Map, fromList, insert, lookup,
                                                  member)
import           Data.Void                  (Void)

import           Text.Megaparsec            (Parsec, between, empty, many,
                                             notFollowedBy, runParser,
                                             takeWhileP, try, (<|>))
import           Text.Megaparsec.Byte       (alphaNumChar, letterChar, space1,
                                             string)
import qualified Text.Megaparsec.Byte.Lexer as L (decimal, lexeme, space,
                                                  symbol)
import           Text.Megaparsec.Error      (parseErrorPretty)
import           Text.Megaparsec.Expr       (Operator (InfixL), makeExprParser)

-------------------------- DATA AND TYPES --------------------------

data Expr = Lit Int
          | Var C.ByteString
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let C.ByteString Expr Expr
    deriving (Show, Eq)

data Action = Creature C.ByteString Expr
            | Assignment C.ByteString Expr
            | Read C.ByteString
            | Write Expr
            | For C.ByteString Expr Expr [Action] Int
            | Break
    deriving (Show, Eq)

data InterprError = ExprDivByZeroError C.ByteString Int
                  | ExprNoVarError C.ByteString C.ByteString Int
                  | CreatureError C.ByteString Int
                  | AssignmentError C.ByteString Int
                  | ReadError C.ByteString Int

class ShowBS a where
    showBS :: a -> C.ByteString

instance ShowBS Int where
    showBS :: Int -> C.ByteString
    showBS x = C.pack $ show x

instance ShowBS InterprError where
    showBS :: InterprError -> C.ByteString
    showBS (ExprDivByZeroError actName num) =
        "(" `C.append` showBS num
        `C.append` "): The expression for "
        `C.append` actName
        `C.append` " contains division by 0"
    showBS (ExprNoVarError failVarName actName num) =
        "(" `C.append` showBS num
        `C.append` "): The expression for "
        `C.append` actName
        `C.append` " can't be fully evaluated because variable \""
        `C.append` failVarName
        `C.append` "\" doesn't exist"
    showBS (CreatureError varName num) =
        "(" `C.append` showBS num
        `C.append` "): The variable \""
        `C.append` varName
        `C.append` "\" can't be created because it already exists"
    showBS (AssignmentError varName num) =
        "(" `C.append` showBS num
        `C.append` "): The variable \""
        `C.append` varName
        `C.append` "\" can't be changed because it doesn't exist"
    showBS (ReadError varName num) =
        "(" `C.append` showBS num
        `C.append` "): The variable \""
        `C.append` varName
        `C.append` "\" can't be readed because it doesn't exist"

type InteprConstraintWithoutContIO m =
    ( MonadError InterprError m
    , MonadState (M.Map C.ByteString Int, Int) m
    )

type InteprConstraintWithoutCont m =
    ( InteprConstraintWithoutContIO m
    , MonadIO m
    )

type InteprConstraint m =
    ( InteprConstraintWithoutCont m
    , MonadCont m
    )

----------------------------- PARSING ------------------------------

type Parser = Parsec Void C.ByteString

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: C.ByteString -> Parser C.ByteString
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

keywordList :: [C.ByteString]
keywordList = ["let", "in", "mut", "for", "from", "to", "break"]

identifier :: Parser C.ByteString
identifier = (lexeme . try) (name >>= isNotKeyword)
  where
    name =
        cons <$> letterChar <*> takeWhileP Nothing (isAlphaNum . chr . fromIntegral)
    isNotKeyword w =
        if w `elem` keywordList
        then fail $ "keyword " ++ show w ++ " can't be a variable name"
        else return w

keyword :: C.ByteString -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

parserLet :: Parser Expr
parserLet = do
    keyword "let"
    varName <- identifier
    symbol "="
    eqExpr <- parserExpr
    keyword "in"
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
    keyword "mut"
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
    keyword "for"
    symbol "("
    varName <- identifier
    keyword "from"
    fromExpr <- parserExpr
    keyword "to"
    toExpr <- parserExpr
    symbol ")"
    symbol "{"
    actions <- parserProgram
    symbol "}"
    return $ For varName fromExpr toExpr actions (getActionsLength actions)
  where
    getActionsLength :: [Action] -> Int
    getActionsLength actions' = sum $ map getActionLength actions'

    getActionLength :: Action -> Int
    getActionLength (For _ _ _ _ len) = 2 + len
    getActionLength _                 = 1

parserBreak :: Parser Action
parserBreak = do
    keyword "break"
    return Break

parserAction :: Parser Action
parserAction =
    parserCreature
    <|> parserAssignment
    <|> parserRead
    <|> parserWrite
    <|> parserFor
    <|> parserBreak

parserProgram :: Parser [Action]
parserProgram = many parserAction

---------------------------- FUNCTIONS -----------------------------

eval :: ( MonadError InterprError m
        , MonadReader (M.Map C.ByteString Int, Int, C.ByteString) m
        )
    => Expr -> m Int
eval (Lit val) = return val
eval (Var name) = do
    (m, num, act) <- ask
    case M.lookup name m of
        Nothing  -> throwError $ ExprNoVarError name act num
        Just val -> return val
eval (Add l r) = liftM2 (+) (eval l) (eval r)
eval (Sub l r) = liftM2 (-) (eval l) (eval r)
eval (Mul l r) = liftM2 (*) (eval l) (eval r)
eval (Div l r) = do
    rCalced <- eval r
    if rCalced == 0
    then do
        (m, num, act) <- ask
        throwError $ ExprDivByZeroError act num
    else do
        lCalced <- eval l
        return $ div lCalced rCalced
eval (Let v eqExpr inExpr) = do
    eqExprCalced <- eval eqExpr
    let changeMap = \(curM, curNum, curAct) -> (M.insert v eqExprCalced curM, curNum, curAct)
    local changeMap (eval inExpr)

varActionWithExcept :: InteprConstraintWithoutContIO m
    => C.ByteString -> Int -> Bool -> (C.ByteString -> Int -> InterprError) -> m ()
varActionWithExcept name val mustBeMember constrError = do
    (m, num) <- get
    if not (M.member name m == mustBeMember)
    then throwError $ constrError name num
    else modify $ \(curM, curNum) -> (M.insert name val curM, curNum + 1)

creature :: InteprConstraintWithoutContIO m
    => C.ByteString -> Int -> m ()
creature name val = varActionWithExcept name val False CreatureError

assignment :: InteprConstraintWithoutContIO m
    => C.ByteString -> Int -> m ()
assignment name val = varActionWithExcept name val True AssignmentError

readVar :: InteprConstraintWithoutCont m
    => C.ByteString -> m ()
readVar name = do
    valStr <- liftIO getLine
    varActionWithExcept name (read valStr) True ReadError

writeExpr :: InteprConstraintWithoutCont m
    => Int  -> m ()
writeExpr val = do
    liftIO $ C.putStrLn (showBS val)
    modify $ \(curM, curNum) -> (curM, curNum + 1)

-------------------------- INTERPRITATION --------------------------

interpritationFor :: ( InteprConstraint m
                     , MonadReader ([Action], Int, C.ByteString, Expr, Int, C.ByteString) m
                     )
    => m ()
interpritationFor = callCC $ \exit -> do
    (m, num) <- get
    (actions, len, name, incExpr, toVal, actName) <- ask
    case M.lookup name m of
        Nothing -> throwError $ AssignmentError name num
        Just counterVal ->
            if counterVal >= toVal
            then
                modify $ \(curM, curNum) -> (curM, curNum + len + 1)
            else do
                mapM_ (maybeInterpritation exit) actions
                interpritationWithOneExpr incExpr (assignment name) actName
                modify $ \(curM, curNum) -> (curM, num)
                interpritationFor
  where
    maybeInterpritation :: InteprConstraint m
        => (() -> m ()) -> Action -> m ()
    maybeInterpritation exit' action = do
        when (action == Break) $ exit' ()
        interpritation action

interpritationWithOneExpr :: InteprConstraintWithoutCont m
    => Expr -> (Int -> m ()) -> C.ByteString -> m ()
interpritationWithOneExpr expr varAction actName = do
    (m, num) <- get
    val <- runReaderT (eval expr) (m, num, actName)
    varAction val

interpritation :: InteprConstraint m
    => Action -> m ()
interpritation (Creature name expr) = do
    let actName = "creature \"" `C.append` name `C.append` "\""
    interpritationWithOneExpr expr (creature name) actName
interpritation (Assignment name expr) = do
    let actName = "assignment \"" `C.append` name `C.append` "\""
    interpritationWithOneExpr expr (assignment name) actName
interpritation (Write expr) =
    interpritationWithOneExpr expr writeExpr "writing"
interpritation (Read name) = readVar name
interpritation (For name fromExpr toExpr actions len) = do
    (m, num) <- get
    let actName = "assignment \"" `C.append` name `C.append` "\""
    let exprEnv = (m, num, actName)
    fromVal <- runReaderT (eval fromExpr) exprEnv
    toVal <- runReaderT (eval toExpr) exprEnv
    assignment name fromVal
    let incExpr = Add (Var name) (Lit 1)
    let forEnv = (actions, len, name, incExpr, toVal, actName)
    runReaderT interpritationFor forEnv
interpritation Break = return ()

fullInterpritation :: InteprConstraint m
    => [Action] -> m ()
fullInterpritation = mapM_ interpritation

----------------------------- STARTING -----------------------------

main :: IO ()
main = do
    path <- getLine
    code <- C.readFile path
    case runParser parserProgram "" code of
        Left parseErr -> putStr $ parseErrorPretty parseErr
        Right prog -> do
            let interpr = fullInterpritation prog
            let state = (M.fromList [], 1)
            let exit = return
            eitherInterpr <- runContT (runStateT (runExceptT interpr) state) exit
            case eitherInterpr of
                (Left interprErr, newSt) -> C.putStrLn $ showBS interprErr
                (Right (), newSt)        -> return ()
