{-# LANGUAGE InstanceSigs #-}

module Block4 where

import           Data.Foldable  (Foldable)
import           Data.Semigroup (Semigroup ((<>)))

------------------------------ TASK 1 ------------------------------

data Pair a = Pair a a
    deriving (Eq, Show)

instance Foldable Pair where
    foldr :: (a -> b -> b) -> b -> Pair a -> b
    foldr f z (Pair x y) = f x (f y z)

    foldMap :: Monoid m => (a -> m) -> Pair a -> m
    foldMap f (Pair x y) = f x `mappend` f y

data NonEmpty a = a :| [a]
    deriving (Eq, Show)

instance Foldable NonEmpty where
    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f z (x1 :| (x2 : xs)) = f x1 (foldr f z (x2 :| xs))
    foldr f z (x :| _)          = f x z

    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (x1 :| (x2 : xs)) = f x1 `mappend` foldMap f (x2 :| xs)
    foldMap f (x :| _)          = f x

-- instance Foldable Tree - in Block3

------------------------------ TASK 2 ------------------------------

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr (split sep) ([] :| [])
  where
    split :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    split s x (acc :| accs) =
        if s == x
        then [] :| (acc : accs)
        else (x : acc) :| accs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep ll = drop 1 (foldl (join sep) [] ll)
  where
    join :: a -> [a] -> [a] -> [a]
    join s acc xl = acc ++ (s : xl)

-------------------- PART TASK 2 FROM BLOCK 5 --------------------

instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    (x :| xs) <> (y :| ys) = x :| (xs ++ (y : ys))
