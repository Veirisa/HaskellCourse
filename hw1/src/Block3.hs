{-# LANGUAGE InstanceSigs #-}

module Block3 where

import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)), cons, length,
                                           toList)

------------------------------ TASK 1 ------------------------------

data DayOfWeek = Monday
               | Tuesday
               | Wednesday
               | Thursday
               | Friday
               | Saturday
               | Sunday
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

nextDay :: DayOfWeek -> DayOfWeek
nextDay day =
    if day == (maxBound :: DayOfWeek)
    then minBound :: DayOfWeek
    else succ day

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays day 0 = day
afterDays day x = afterDays (nextDay day) (x - 1)

isWeekend :: DayOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: DayOfWeek -> Int
daysToParty Friday = 0
daysToParty day    = daysToParty (nextDay day) + 1

------------------------------ TASK 2 ------------------------------

data HousePeople = Alone | Two | Three | Four
    deriving Eq
newtype House = House HousePeople
    deriving Eq
newtype Houses = Houses (NE.NonEmpty House)
    deriving Eq
data Building = Church | Library
    deriving  Eq
data Special = Special Building | SpecialEmpty
    deriving Eq
data Lord = Lord
    deriving Eq
data Castle = CastleWithLord Lord | Castle
    deriving Eq
data Walls = Walls
    deriving Eq
data FortressWalls = FortressWalls Walls | FortressWallsEmpty
    deriving Eq
data Fortress = Fortress Castle FortressWalls | FortressEmpty
    deriving Eq
data City = City Fortress Special Houses
    deriving Eq

createCastle :: City -> (City, Bool)
createCastle (City FortressEmpty special houses) =
    ((City (Fortress Castle FortressWallsEmpty) special houses), True)
createCastle city = (city, False)

createSpecial :: City -> Building -> (City,  Bool)
createSpecial (City fortress SpecialEmpty houses) building =
    ((City fortress (Special building) houses), True)
createSpecial city _ = (city, False)

createHouse :: City -> HousePeople -> City
createHouse (City fortress special (Houses listHouse)) people =
    City fortress special (Houses (NE.cons (House people) listHouse))

data LordStatus = Ok | HaveNotCastle | JustHaveLord
    deriving (Eq, Show)

setLord :: City -> Lord -> (City, LordStatus)
setLord (City (Fortress Castle fortressWalls) special houses) lord =
    (City (Fortress (CastleWithLord lord) fortressWalls) special houses, Ok)
setLord city@(City FortressEmpty _ _) _ = (city, HaveNotCastle)
setLord city _ = (city, JustHaveLord)

createWalls :: City -> (City, Bool)
createWalls city@(City fortress special houses@(Houses listHouse)) =
    if countPeople 0 (NE.toList listHouse) < 10
    then (city, False)
    else
      let
        (newFortress, wallsCreated) = doCreateWalls fortress
      in
        if wallsCreated == True
        then (City newFortress special houses, True)
        else (city, False)
  where
    countPeople :: Int -> [House] -> Int
    countPeople acc ((House Alone) : hs) = countPeople (acc + 1) hs
    countPeople acc ((House Two) : hs)   = countPeople (acc + 2) hs
    countPeople acc ((House Three) : hs) = countPeople (acc + 3) hs
    countPeople acc ((House Four) : hs)  = countPeople (acc + 4) hs
    countPeople acc _                    = acc

    doCreateWalls :: Fortress -> (Fortress, Bool)
    doCreateWalls (Fortress castle@(CastleWithLord _) FortressWallsEmpty) =
        (Fortress castle (FortressWalls Walls), True)
    doCreateWalls oldFortress = (oldFortress, False)

------------------------------ TASK 3 ------------------------------

data Nat = Z | S Nat

instance Num Nat where
    (+) :: Nat -> Nat -> Nat
    x + Z = x
    x + S yy = S x + yy

    (-) :: Nat -> Nat -> Nat
    Z - _ = Z
    x - Z = x
    S xx - S yy = xx - yy

    (*) :: Nat -> Nat -> Nat
    Z * _ = Z
    _ * Z = Z
    x * S yy = x + x * yy

    abs :: Nat -> Nat
    abs x = x

    signum :: Nat -> Nat
    signum Z = Z
    signum _ = S Z

    fromInteger :: Integer -> Nat
    fromInteger x
        | x < 0     = error "negative number is not a natural number"
        | x <= 0    = Z
        | otherwise = S (fromInteger (x - 1))

instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    Z == Z       = True
    S xx == S yy = xx == yy
    _ == _       = False

instance Ord Nat where
    compare :: Nat -> Nat -> Ordering
    compare Z Z           = EQ
    compare Z _           = LT
    compare _ Z           = GT
    compare (S xx) (S yy) = compare xx yy

toIntegerNat :: Nat -> Integer
toIntegerNat Z      = 0
toIntegerNat (S xx) = 1 + toIntegerNat xx

isEvenNat :: Nat -> Bool
isEvenNat Z           = True
isEvenNat (S (S xxx)) = isEvenNat xxx
isEvenNat _           = False

divModNat :: Nat -> Nat -> (Nat, Nat)
divModNat x y = doDivModNat x y Z
  where
    doDivModNat :: Nat -> Nat -> Nat -> (Nat, Nat)
    doDivModNat x1 y1 divAcc
        | y1 == Z    = error "div by zero"
        | x1 < y1     = (divAcc, x1)
        | otherwise = doDivModNat (x1 - y1) y1 (S divAcc)

divNat :: Nat -> Nat -> Nat
divNat x y = fst (divModNat x y)

modNat :: Nat -> Nat -> Nat
modNat x y = snd (divModNat x y)

------------------------------ TASK 4 ------------------------------

data Tree a = Leaf | Node (NE.NonEmpty a) (Tree a) (Tree a)
    deriving Eq

treeIsEmpty :: Tree a -> Bool
treeIsEmpty Leaf = True
treeIsEmpty _    = False

treeSize :: Tree a -> Int
treeSize Leaf = 0
treeSize (Node values left right) =
    NE.length values + treeSize left + treeSize right

treeSearch :: Ord a => Tree a -> a -> Bool
treeSearch Leaf _ = False
treeSearch (Node (val NE.:| _) left right) x =
    if x == val
    then True
    else
        if x < val
        then treeSearch left x
        else treeSearch right x

treeInsert :: Ord a => Tree a -> a -> Tree a
treeInsert Leaf x = Node (x NE.:| []) Leaf Leaf
treeInsert (Node values@(val NE.:| _) left right) x =
    if x == val
    then Node (NE.cons x values) left right
    else
        if x < val
        then Node values (treeInsert left x) right
        else Node values left (treeInsert right x)

treeRemove :: Ord a => Tree a -> a -> Tree a
treeRemove tree@(Leaf) _ = tree
treeRemove tree@(Node values@(val NE.:| _) left right) x =
    if x == val
    then doTreeRemove tree
    else
        if x < val
        then Node values (treeRemove left x) right
        else Node values left (treeRemove right x)
  where
    modifyRight :: Tree a -> (NE.NonEmpty a, Tree a)
    modifyRight (Node chValues Leaf chRight) = (chValues, chRight)
    modifyRight (Node chValues chLeft chRight) =
      let
        (upperBound, newLeft) = modifyRight chLeft
      in
        (upperBound, Node chValues newLeft chRight)

    doTreeRemove :: Tree a -> Tree a
    doTreeRemove (Node (_ NE.:| (v2:vs)) curLeft curRight) =
        Node (v2 NE.:| vs) curLeft curRight
    doTreeRemove (Node _ curLeft Leaf) = curLeft
    doTreeRemove (Node _ curLeft curRight) =
      let
        (upperBound, newRight) = modifyRight curRight
      in
        Node upperBound curLeft newRight

fromList :: Ord a => [a] -> Tree a
fromList l = treeFromList Leaf l
  where
    treeFromList :: Ord a => Tree a -> [a] -> Tree a
    treeFromList tree (x : xs) = treeFromList (treeInsert tree x) xs
    treeFromList tree _        = tree
