{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (liftM2, void)
import           Control.Monad.Cont         (MonadCont, callCC, when)
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
import qualified Text.Megaparsec.Byte.Lexer as L (decimal, lexeme, signed,
                                                  space, symbol)
import           Text.Megaparsec.Error      (parseErrorPretty)
import           Text.Megaparsec.Expr       (Operator (InfixL), makeExprParser)

import           System.Environment         (getArgs)

-------------------------- DATA AND TYPES --------------------------

type BString = C.ByteString

data Expr = Lit Int
          | Var BString
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let BString Expr Expr
    deriving (Show, Eq)

data Action = Create BString Expr
            | Assign BString Expr
            | Read BString
            | Write Expr
            | For BString Expr Expr [Action] Int
            | Break
    deriving (Show, Eq)

data InterprError = ExprDivByZeroError BString Int
                  | ExprNoVarError BString BString Int
                  | CreateError BString Int
                  | AssignError BString Int
                  | ReadError BString Int

class ShowBS a where
    showBS :: a -> BString

instance ShowBS Int where
    showBS :: Int -> BString
    showBS x = C.pack $ show x

instance ShowBS InterprError where
    showBS :: InterprError -> BString
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
    showBS (CreateError varName num) =
        "(" `C.append` showBS num
        `C.append` "): The variable \""
        `C.append` varName
        `C.append` "\" can't be created because it already exists"
    showBS (AssignError varName num) =
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
    , MonadState (M.Map BString Int, Int) m
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

type Parser = Parsec Void BString

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: BString -> Parser ()
symbol = void . L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme $ L.signed (L.space empty empty empty) L.decimal

identifier :: Parser BString
identifier = (lexeme . try) (name >>= isNotKeyword)
  where
    name :: Parser BString
    name = cons <$> letterChar <*> takeWhileP Nothing (isAlphaNum . chr . fromIntegral)

    isNotKeyword :: BString -> Parser BString
    isNotKeyword w =
        if w `elem` keywordList
        then fail $ "keyword " ++ show w ++ " can't be a variable name"
        else return w

    keywordList :: [BString]
    keywordList = ["let", "in", "mut", "for", "from", "to", "break"]

keyword :: BString -> Parser ()
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

parserCreateOrAssign :: (BString -> Expr -> Action) -> Parser Action
parserCreateOrAssign constrAction = do
    varName <- identifier
    symbol "="
    expr <- parserExpr
    return $ constrAction varName expr

parserCreate :: Parser Action
parserCreate = do
    keyword "mut"
    parserCreateOrAssign Create

parserAssign :: Parser Action
parserAssign = parserCreateOrAssign Assign

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
    symbol  "("
    varName <- identifier
    keyword "from"
    fromExpr <- parserExpr
    keyword "to"
    toExpr <- parserExpr
    symbol  ")"
    symbol  "{"
    actions <- parserProgram
    symbol  "}"
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
    parserCreate
    <|> parserAssign
    <|> parserRead
    <|> parserWrite
    <|> parserFor
    <|> parserBreak

parserProgram :: Parser [Action]
parserProgram = many parserAction

---------------------------- FUNCTIONS -----------------------------

eval :: ( MonadError InterprError m
        , MonadReader (M.Map BString Int, Int, BString) m
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
        (_, num, act) <- ask
        throwError $ ExprDivByZeroError act num
    else do
        lCalced <- eval l
        return $ div lCalced rCalced
eval (Let v eqExpr inExpr) = do
    eqExprCalced <- eval eqExpr
    let changeEnv (m', num', act') = (M.insert v eqExprCalced m', num', act')
    local changeEnv (eval inExpr)

actionWithExcept :: InteprConstraintWithoutContIO m
    => BString -> Int -> Bool -> (BString -> Int -> InterprError) -> m ()
actionWithExcept name val mustBeMember constrError = do
    (m, num) <- get
    if M.member name m /= mustBeMember
    then throwError $ constrError name num
    else modify $ \(curM, curNum) -> (M.insert name val curM, curNum + 1)

create :: InteprConstraintWithoutContIO m
    => BString -> Int -> m ()
create name val = actionWithExcept name val False CreateError

assign :: InteprConstraintWithoutContIO m
    => BString -> Int -> m ()
assign name val = actionWithExcept name val True AssignError

readVar :: InteprConstraintWithoutCont m
    => BString -> m ()
readVar name = do
    valStr <- liftIO getLine
    actionWithExcept name (read valStr) True ReadError

writeExpr :: InteprConstraintWithoutCont m
    => Int  -> m ()
writeExpr val = do
    liftIO $ C.putStrLn (showBS val)
    modify $ \(curM, curNum) -> (curM, curNum + 1)

-------------------------- INTERPRITATION --------------------------

interpritationFor :: ( InteprConstraint m
                     , MonadReader ([Action], Int, BString, Expr, Int, BString) m
                     )
    => m ()
interpritationFor = callCC $ \exit -> do
    (m, num) <- get
    (actions, len, name, incExpr, toVal, actName) <- ask
    case M.lookup name m of
        Nothing -> throwError $ AssignError name num
        Just counterVal ->
            if counterVal >= toVal
            then modify $ \(curM, curNum) -> (curM, curNum + len + 1)
            else do
                mapM_ (maybeInterpritation exit) actions
                interpritationWithOneExpr incExpr (assign name) actName
                modify $ \(curM, _) -> (curM, num)
                interpritationFor
  where
    maybeInterpritation :: InteprConstraint m
        => (() -> m ()) -> Action -> m ()
    maybeInterpritation exit' action = do
        when (action == Break) $ exit' ()
        interpritation action

interpritationWithOneExpr :: InteprConstraintWithoutCont m
    => Expr -> (Int -> m ()) -> BString -> m ()
interpritationWithOneExpr expr valAction actName = do
    (m, num) <- get
    val <- runReaderT (eval expr) (m, num, actName)
    valAction val

interpritation :: InteprConstraint m
    => Action -> m ()
interpritation (Create name expr) = do
    let actName = "creature \"" `C.append` name `C.append` "\""
    interpritationWithOneExpr expr (create name) actName
interpritation (Assign name expr) = do
    let actName = "assignment \"" `C.append` name `C.append` "\""
    interpritationWithOneExpr expr (assign name) actName
interpritation (Write expr) =
    interpritationWithOneExpr expr writeExpr "writing"
interpritation (Read name) = readVar name
interpritation (For name fromExpr toExpr actions len) = do
    (m, num) <- get
    let actName = "assignment \"" `C.append` name `C.append` "\""
    let exprEnv = (m, num, actName)
    fromVal <- runReaderT (eval fromExpr) exprEnv
    toVal <- runReaderT (eval toExpr) exprEnv
    assign name fromVal
    let incExpr = Add (Var name) (Lit 1)
    let forEnv = (actions, len, name, incExpr, toVal, actName)
    runReaderT interpritationFor forEnv
interpritation Break = return ()

fullInterpritation :: InteprConstraint m
    => [Action] -> m ()
fullInterpritation = mapM_ interpritation

---------------------------- EXECUTION -----------------------------

execProgram :: FilePath -> IO ()
execProgram path = do
    code <- C.readFile path
    case runParser parserProgram "" code of
        Left parseErr -> putStr $ parseErrorPretty parseErr
        Right prog -> do
            let interpr = fullInterpritation prog
            let state = (M.fromList [], 1)
            let exit = return
            eitherInterpr <- runContT (runStateT (runExceptT interpr) state) exit
            case eitherInterpr of
                (Left interprErr, _) -> C.putStrLn $ showBS interprErr
                (Right (), _)        -> return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> execProgram path
        _      -> putStrLn "Wrong number of arguments"
