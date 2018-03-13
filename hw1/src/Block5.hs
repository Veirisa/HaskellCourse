{-# LANGUAGE InstanceSigs #-}

module Block5 where

import           Block4         (NonEmpty ((:|)))
import           Data.Maybe
import           Data.Semigroup hiding (Endo)

------------------------------ TASK 1 ------------------------------

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat ll = fromMaybe [] (mconcat ll)

eitherConcat :: (Monoid m1, Monoid m2, Foldable t) => t (Either m1 m2) -> (m1, m2)
eitherConcat ll = foldr joinMonoids (mempty, mempty) ll
  where
    joinMonoids :: (Monoid m1, Monoid m2) => (Either m1 m2) -> (m1, m2) -> (m1, m2)
    joinMonoids (Left x) (l, r)  = (x `mappend` l, r)
    joinMonoids (Right y) (l, r) = (l, y `mappend` r)

------------------------------ TASK 2 ------------------------------

instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    (x :| xs) <> (y :| ys) = x :| (xs ++ (y : ys))

data ThisOrThat a b = This a | That b | Both a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
    (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
    This x1 <> This x2       = This (x1 <> x2)
    This x1 <> That y2       = Both x1 y2
    This x1 <> Both x2 y2    = Both (x1 <> x2) y2
    That y1 <> This x2       = Both x2 y1
    That y1 <> That y2       = That (y1 <> y2)
    That y1 <> Both x2 y2    = Both x2 (y1 <> y2)
    Both x1 y1 <> This x2    = Both (x1 <> x2) y1
    Both x1 y1 <> That y2    = Both x1 (y1 <> y2)
    Both x1 y1 <> Both x2 y2 = Both (x1 <> x2) (y1 <> y2)

data Name = Name String | NameEmpty
    deriving (Eq, Show)

instance Semigroup Name where
    (<>) :: Name -> Name -> Name
    Name s1 <> Name s2 = Name (s1 ++ ('.' : s2))
    n1@(Name s1) <> _  = n1
    _ <> n2            = n2

instance Monoid Name where
    mempty :: Name
    mempty = NameEmpty

    mappend :: Name -> Name -> Name
    n1 `mappend` n2 = n1 <> n2

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    (<>) :: Endo a -> Endo a -> Endo a
    Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
    mempty :: Endo a
    mempty = Endo id

    mappend :: Endo a -> Endo a -> Endo a
    ef `mappend` eg = ef <> eg

------------------------------ TASK 3 ------------------------------

data Builder = One Char | Many [Builder]
    deriving (Show)

fromString :: String -> Builder
fromString s = Many (map (\x -> One x) s)

toString :: Builder -> String
toString (One c)   = [c]
toString (Many lb) = concatMap getString lb
  where
    getString (One x) = [x]
    getString b       = toString b

instance Eq Builder where
    (==) :: Builder -> Builder -> Bool
    b1 == b2 = toString b1 == toString b2

instance Semigroup Builder where
    (<>) :: Builder -> Builder -> Builder
    Many lb1 <> Many lb2 = Many (lb1 ++ lb2)
    b1 <> Many lb2       = Many (b1 : lb2)
    Many lb1 <> b2       = Many (lb1 ++ [b2])
    b1 <> b2             = Many [b1, b2]

instance Monoid Builder where
    mempty :: Builder
    mempty = Many []

    mappend :: Builder -> Builder -> Builder
    b1 `mappend` b2 = b1 <> b2
