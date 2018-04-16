{-# LANGUAGE InstanceSigs #-}

module Block3 where

------------------------------ TASK 1 ------------------------------

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser parser) = Parser (fmap (first f) . parser)
      where
        first :: (a -> b) -> (a,c) -> (b,c)
        first f (x, y) = (f x, y)

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure a = Parser $ \l -> Just (a, l)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    Parser pf <*> Parser pa = Parser $ \l -> case pf l of
        Nothing     -> Nothing
        Just (f, t) -> case pa t of
            Nothing     -> Nothing
            Just (a, r) -> Just (f a, r)

-- instance Alternative (Parser s) where
--    empty :: Parser a
--    (<|>) :: Parser a -> Parser a -> Parser a

-- instance Monad Parser

------------------------------ TASK 2 ------------------------------

ok :: Parser s ()
ok = Parser $ \l -> Just ((), l)

eof :: Parser s ()
eof = Parser $ \l -> case l of
    [] -> Just ((), l)
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \l -> case l of
    []     -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

element :: Eq s => s -> Parser s s
element el = satisfy (== el)

stream :: Eq s => [s] -> Parser s [s]
stream subl = Parser $ \l -> case removeSubl subl l of
    Nothing   -> Nothing
    Just resl -> Just (subl, resl)
  where
    removeSubl :: Eq s => [s] -> [s] -> Maybe [s]
    removeSubl [] l              = Just l
    removeSubl subl []           = Nothing
    removeSubl (x : xs) (y : ys) = if x == y then removeSubl xs ys else Nothing

------------------------------ TASK 3 ------------------------------

------------------------------ TASK 4 ------------------------------
