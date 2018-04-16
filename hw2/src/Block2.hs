{-# LANGUAGE InstanceSigs #-}

module Block2 where

import           Control.Monad (liftM2)
import           Data.Char     (isDigit)

------------------------------ TASK 1 ------------------------------

stringSum :: String -> Maybe Int
stringSum s = saveSum (map tryRead (words s))
  where
    tryRead :: String -> Maybe Int
    tryRead s@(x : xs) =
        if (x == '-' || isDigit x) && all isDigit xs
        then Just (read s)
        else Nothing
    tryRead _ = error "impossible situation"

    saveSum :: [Maybe Int] -> Maybe Int
    saveSum a = foldr (liftM2 (+)) (Just 0) a

------------------------------ TASK 2 ------------------------------

data Optional a = Optional (Maybe (Maybe a))

-- fmap id  ==  id
-- fmap (f . g)  ==  fmap f . fmap g
instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap f (Optional (Just mx)) = Optional (Just (fmap f mx))
    fmap _ _                    = Optional Nothing

-- fmap f x = pure f <*> x
-- liftA2 p (liftA2 q u v) = liftA2 f u . liftA2 g v
instance Applicative Optional where
    pure :: a -> Optional a
    pure x = Optional (pure (pure x))

    (<*>) :: Optional (a -> b) -> Optional a -> Optional b
    Optional (Just (Just f)) <*> opx = fmap f opx
    Optional (Just Nothing) <*> _    = Optional (Just Nothing)
    Optional Nothing <*> _           = Optional Nothing

instance Monad Optional where
    return :: a -> Optional a
    return x = Optional (return (return x))

    (>>=) :: Optional a -> (a -> Optional b) -> Optional b
    Optional (Just (Just x)) >>= f = f x
    Optional (Just Nothing) >>= _  = Optional (Just Nothing)
    _ >>= _                        = Optional Nothing

instance Foldable Optional where
    foldr :: (a -> b -> b) -> b -> Optional a -> b
    foldr f y (Optional (Just (Just x))) = f x y
    foldr f y _                          = y
