{-# LANGUAGE InstanceSigs #-}

module Block4 where

import           Block3        (Tree (Leaf, Node))
import           Data.Foldable (Foldable)

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

instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ z Leaf = z
    foldr f z (Node values left right) =
        foldr f (foldr f (foldr f z right) values) left

    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Leaf = mempty
    foldMap f (Node values left right) =
        foldMap f left `mappend` foldMap f values `mappend` foldMap f right

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
    join s xl acc = xl ++ (s : acc)
