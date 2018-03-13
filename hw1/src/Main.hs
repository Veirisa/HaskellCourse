module Main where

import           Block1
import           Block2
import           Block3
import           Block4
import           Block5
import           Data.Foldable
import           Data.List          (sort)
import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)), head)
import           Data.Semigroup     hiding (Endo)
import           System.Random      (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO[Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

verdict :: Bool -> String
verdict True = "OK"
verdict _    = "FAIL"

main :: IO ()
main = putStrLn (test1 ++ ('\n':test2) ++ ('\n':test3) ++ ('\n':test4)
                 ++ ('\n':test5) ++ "\n")

------------------------------ BLOCK 1 ------------------------------

test1 =
    "\n------ Block 1 ------"
    ++ ('\n':test11) ++ ('\n':test12) ++ ('\n':test13) ++ ('\n':test14)

test11 =
    "\n1|  order3: "
    ++ verdict (order3 (5, 2, 10) == (2, 5, 10))

test12 =
    "\n2|  smartReplicate: "
    ++ verdict (smartReplicate [1,2,3] == [1,2,2,3,3,3])

test13 =
    "\n3|  contains: "
    ++ verdict (contains 3 [[1..5], [2,0], [3,4]] == [[1,2,3,4,5],[3,4]])

test14 =
    "\n4|  stringSum: "
    ++ verdict (and (map check tests))
  where
    check test = stringSum (fst test) == snd test
    tests = [("1 1", 2),
             ("100\n\t-3", 97),
             ("1", 1),
             ("1 2 3", 6),
             (" 1", 1),
             ( "1 ", 1),
             ("\t1\t", 1),
             ("\t12345\t", 12345),
             ("010 020 030", 60),
             (" 123 456 789 ", 1368),
             ("-1", (-1)),
             ("-1 -2 -3", (-6)),
             ("\t-12345\t", (-12345)),
             (" -123 -456 -789 ", (-1368)),
             ("\n1\t\n3   555  -1\n\n\n-5", 553),
             ("123\t\n\t\n\t\n321 -4 -40", 400)]

------------------------------ BLOCK 2 ------------------------------

test2 =
    "\n------ Block 2 ------"
    ++ ('\n':test21) ++ ('\n':test22)

test21 =
    "\n1|  removeFromList: "
    ++ verdict (removeFromList 1 [1, 2, 3] == (2, [1, 3]))

test22 =
    "\n2|  mergeSort: "
    ++ verdict (and (map check testsNum ++ map check testsChar))
  where
    testsNum = [[2, 1, 0, 3, 10, 5],
             [10, (-10), 5, (-5), 0, (-20), (-30), 15, 10]]
    testsChar = [['a', 'z', 'x', 'y', 'f', 'z', 'r']]
    check test = sort test == mergeSort test

------------------------------ BLOCK 3 ------------------------------

test3 =
    "\n------ Block 3 ------"
    ++ ('\n':test31) ++ ('\n':test32) ++ ('\n':test33) ++ ('\n':test34)

test31 =
    "\n1|  DayOfWeek"
    ++ "\n    nextDay: "
    ++ verdict (nextDay Wednesday == Thursday && nextDay Sunday == Monday)
    ++ "\n    afterDays: "
    ++ verdict (afterDays Monday 4 == Friday && afterDays Saturday 3 == Tuesday)
    ++ "\n    isWeekend: "
    ++ verdict (not (isWeekend Friday) && isWeekend Sunday)
    ++ "\n    daysToParty: "
    ++ verdict (daysToParty Monday == 4 && daysToParty Saturday == 6)

test32 =
    "\n2|  City"
    ++ "\n    createCastle: "
    ++ verdict (createCastle city == (cityCas, True)
                && createCastle cityCas == (cityCas, False))
    ++ "\n    createSpecial: "
    ++ verdict (createSpecial city Library == (cityLib, True)
                && createSpecial cityLib Church == (cityLib, False))
    ++ "\n    createHouse: "
    ++ verdict (createHouse citySh Four == city)
    ++ "\n    setLord: "
    ++ verdict (setLord city Lord == (city, HaveNotCastle)
                && setLord cityCasLord Lord == (cityCasLord, JustHaveLord)
                && setLord cityCas Lord == (cityCasLord, Ok))
    ++ "\n    createWalls: "
    ++ verdict (createWalls cityCas == (cityCas, False)
                && createWalls cityCasLordSh == (cityCasLordSh, False)
                && createWalls cityCasLordWalls == (cityCasLordWalls, False)
                && createWalls cityCasLord == (cityCasLordWalls, True))

  where
    smallHouses = Houses (House Four NE.:| [House Three, House Two])
    houses = Houses (House Four NE.:| [House Four, House Three, House Two])

    city = City FortressEmpty SpecialEmpty houses
    citySh = City FortressEmpty SpecialEmpty smallHouses
    cityCas = City (Fortress Castle FortressWallsEmpty) SpecialEmpty houses
    cityLib = City FortressEmpty (Special Library) houses
    cityCasLord =
        City (Fortress (CastleWithLord Lord) FortressWallsEmpty) SpecialEmpty houses
    cityCasLordSh =
        City (Fortress (CastleWithLord Lord) FortressWallsEmpty) SpecialEmpty smallHouses
    cityCasLordWalls =
        City (Fortress (CastleWithLord Lord) (FortressWalls Walls)) SpecialEmpty houses

test33 =
    "\n3|  Nat"
    ++ "\n    toIntegerNat: "
    ++ verdict (toIntegerNat Z == 0 && toIntegerNat (S (S (S (S (S Z))))) == 5)
    ++ "\n    fromInteger: "
    ++ verdict (fromInteger 0 == Z && fromInteger 5 == S (S (S (S (S Z)))))
    ++ "\n    +: "
    ++ verdict (checkBinOp (+) 40 60 100)
    ++ "\n    -: "
    ++ verdict (checkBinOp (-) 40 60 0 && checkBinOp (-) 60 40 20)
    ++ "\n    *: "
    ++ verdict (checkBinOp (*) 4 0 0 && checkBinOp (*) 0 5 0
                && checkBinOp (*) 4 5 20)
    ++ "\n    abs: "
    ++ verdict (checkUnOp abs 10 10 && checkUnOp abs 0 0)
    ++ "\n    signum: "
    ++ verdict (checkUnOp signum 10 1 && checkUnOp signum 0 0)
    ++ "\n    ==: "
    ++ verdict (checkEqual 10 10 True && checkEqual 10 5 False
                && checkEqual 5 10 False)
    ++ "\n    compare: "
    ++ verdict (checkCompare 5 10 LT && checkCompare 10 10 EQ
                && checkCompare 10 5 GT)
    ++ "\n    isEvenNat: "
    ++ verdict (checkEven 5 False && checkEven 10 True)
    ++ "\n    divNat: "
    ++ verdict (checkBinOp divNat 10 5 2 && checkBinOp divNat 9 4 2
                && checkBinOp divNat 5 10 0 && checkBinOp divNat 0 10 0)
    ++ "\n    modNat: "
    ++ verdict (checkBinOp modNat 10 5 0 && checkBinOp modNat 9 4 1
                && checkBinOp modNat 5 10 5 && checkBinOp modNat 0 10 0)
  where
    checkUnOp op x res = toIntegerNat (op (fromInteger x)) == res
    checkBinOp op x y res = toIntegerNat (fromInteger x `op` fromInteger y) == res
    checkEven x res = isEvenNat (fromInteger x) == res
    checkEqual x y res = (fromInteger x == fromInteger y) == res
    checkCompare x y res = compare (fromInteger x) (fromInteger y) == res


test34 =
    "\n4|  Tree"
    ++ "\n    treeIsEmpty: "
    ++ verdict (treeIsEmpty Leaf && not (treeIsEmpty tree))
    ++ "\n    treeSize: "
    ++ verdict (treeSize Leaf == 0 && treeSize tree == 7
                && treeSize treeMulti == 10)
    ++ "\n    treeSearch: "
    ++ verdict (treeSearch tree 3 && treeSearch treeMulti 3
                && treeSearch tree 5 && not (treeSearch treeMulti 5)
                && not (treeSearch treeMulti 8) && not (treeSearch treeMulti 8))
    ++ "\n    treeInsert: "
    ++ verdict (checkInsert tree 8 && checkInsert treeMulti 8
                && checkInsert tree 3 && checkInsert treeMulti 3)
    ++ "\n    treeRemove: "
    ++ verdict (treeRemove tree 8 == tree && treeRemove treeMulti 8 == treeMulti
                && checkRemove tree False [1..7]
                && checkRemove treeMulti False [1]
                && checkRemove treeMulti True [2..4])
    ++ "\n    fromList: "
    ++ verdict (fromList [4, 2, 1, 3, 6, 5, 7] == tree
                && fromList [2, 2, 1, 3, 3, 3, 4, 4, 4, 4] == treeMulti
                && fromList [2, 3, 1, 4, 2, 3, 4, 3, 4, 4] == treeMulti)
  where
    tree =
        Node (4 NE.:| [])
            (Node (2 NE.:| [])
                (Node (1 NE.:| []) Leaf Leaf)
                (Node (3 NE.:| []) Leaf Leaf))
            (Node (6 NE.:| [])
                (Node (5 NE.:| []) Leaf Leaf)
                (Node (7 NE.:| []) Leaf Leaf))
    treeMulti =
        Node (2 NE.:| [2])
            (Node (1 NE.:| []) Leaf Leaf)
            (Node (3 NE.:| [3, 3])
                Leaf
                (Node (4 NE.:| [4, 4, 4]) Leaf Leaf))

    checkRemove :: Ord a => Tree a -> Bool -> [a] -> Bool
    checkRemove t exist l = and (map (doCheckRemove t exist) l)
      where
        doCheckRemove :: Ord a => Tree a -> Bool -> a -> Bool
        doCheckRemove t exist x =
          let
            newTree = treeRemove t x
          in
            treeSize newTree == treeSize t - 1 && treeSearch newTree x == exist
            && checkInvariant newTree

    checkInsert :: Ord a => Tree a -> a -> Bool
    checkInsert t x =
      let
        newTree = treeInsert t x
      in
        treeSize newTree == treeSize t + 1 && treeSearch newTree x
        && checkInvariant newTree

    checkInvariant :: Ord a => Tree a -> Bool
    сheckInvariant Leaf = True
    checkInvariant (Node vs Leaf Leaf) = True
    checkInvariant (Node (v NE.:|vs) l@(Node (lv NE.:| lvs) ll lr) Leaf) =
       (v > lv) && checkInvariant l
    checkInvariant (Node (v NE.:|vs) Leaf r@(Node (rv NE.:| rvs) rl rr)) =
       (v < rv) && checkInvariant r
    checkInvariant (Node (v NE.:|vs) l@(Node (lv NE.:| lvs) ll lr)
                   r@(Node (rv NE.:| rvs) rl rr)) =
       (v > lv) && (v < rv) && checkInvariant l && checkInvariant r

------------------------------ BLOCK 4 ------------------------------

test4 =
    "\n------ Block 4 ------"
    ++ ('\n':test41) ++ ('\n':test42)

test41 =
    "\n1|  Foldable"
    ++ "\n    (Pair) foldr: "
    ++ verdict (checkFoldr (Pair 2 3) [2, 3])
    ++ "\n    (Pair) foldMap: "
    ++ verdict (checkFoldMap (Pair 'o' 'k') "ok")
    ++ "\n    (NonEmpty) foldr: "
    ++ verdict (checkFoldr (2 :| [3, 4, 5]) [2, 3, 4, 5])
    ++ "\n    (NonEmpty) foldMap: "
    ++ verdict (checkFoldMap ('g' :| "ood") "good")
    ++ "\n    (Tree) foldr: "
    ++ verdict (checkFoldr (fromList [4, 2, 1, 3, 6, 5, 7]) [1, 2, 3, 4, 5, 6, 7])
    ++ "\n    (Tree) foldMap: "
    ++ verdict (checkFoldMap (fromList [4, 2, 1, 3, 6, 5, 7]) [1, 2, 3, 4, 5, 6, 7])
    ++ "\n    (Tree) toList: "
    ++ verdict ((toList . fromList) [2, 3, 1, 4, 2, 3, 4, 3, 4, 4]
                == [1, 2, 2, 3, 3, 3, 4, 4, 4, 4])
  where
    checkFoldr :: (Eq a, Num a, Foldable t1, Foldable t2) => t1 a -> t2 a -> Bool
    checkFoldr x y = foldr (-) 1 x == foldr (-) 1 y

    checkFoldMap :: (Eq a, Foldable t1, Foldable t2) => t1 a -> t2 a -> Bool
    checkFoldMap x y = foldMap (\z -> [z]) x == foldMap (\z -> [z]) y

test42 =
  "\n2|  splitOn: "
  ++ verdict (splitOn '/' "path/to/file" == ("path" :| ["to", "file"]))
  ++ "\n    joinWith: "
  ++ verdict (joinWith '/' ("path" :| ["to", "file"]) == "path/to/file"
              && (joinWith '/' . splitOn '/') "https://hackmd.io/s/rJXa7ir9Z#"
              == "https://hackmd.io/s/rJXa7ir9Z#")

------------------------------ BLOCK 5 ------------------------------

test5 =
    "\n------ Block 5 ------"
    ++ ('\n':test51) ++ ('\n':test52) ++ ('\n':test53)

test51 =
    "\n1|  maybeConcat: "
    ++ verdict (maybeConcat [Just [1,2,3], Nothing, Just [4,5]] == [1,2,3,4,5])
    ++ "\n    eitherConcat: "
    ++ verdict (eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
                == (Sum {getSum = 8}, [1,2,3,4,5]))

test52 =
    "\n2|  Semigroup, Monoid"
    ++ "\n    (NonEmpty) <>: "
    ++ verdict (checkSemigroup (1 :| []) (2 :| [3]) (3 :| [4, 5]))
    ++ "\n    (ThisOrThat) <>: "
    ++ verdict (checkSemigroup (This [1]) (That ['a']) (Both [2] ['b']))
    ++ "\n    (Name) <>: "
    ++ verdict (checkSemigroup NameEmpty (Name "a") (Name "bcdf"))
    ++ "\n    (Name) mempty, mappend: "
    ++ verdict (checkMonoid NameEmpty (Name "a") (Name "bcdf"))
    ++ "\n    (Endo) <>: "
    ++ verdict (checkSemigroupEndo (Endo (\x -> x - 1)) (Endo (\x -> x - 2))
                                   (Endo (\x -> x - 3)))
    ++ "\n    (Endo) mempty, mappend: "
    ++ verdict (checkMonoidEndo (Endo (\x -> x - 1)) (Endo (\x -> x - 2))
                                (Endo (\x -> x - 3)))
  where
    checkSemigroup :: (Eq a, Semigroup a) => a -> a -> a -> Bool
    checkSemigroup x y z = ((x <> y) <> z) == (x <> (y <> z))

    checkMonoid :: (Eq a, Monoid a) => a -> a -> a -> Bool
    checkMonoid x y z = x `mappend` mempty == x
                        && mempty `mappend` y == y
                        && mconcat [x, y, z]  == foldr mappend mempty [x, y, z]
                        && (x `mappend` y) `mappend` z
                        == x `mappend` (y `mappend` z)

    eqSubEndo :: (Eq a, Num a) => Endo a -> Endo a -> Bool
    eqSubEndo x y = getEndo x 0 == getEndo y 0

    checkSemigroupEndo :: (Num a, Eq a) => Endo a -> Endo a -> Endo a -> Bool
    checkSemigroupEndo x y z = eqSubEndo ((x <> y) <> z) (x <> (y <> z))

    checkMonoidEndo :: (Num a, Eq a) => Endo a -> Endo a -> Endo a -> Bool
    checkMonoidEndo x y z = eqSubEndo (x `mappend` mempty) x
                            && eqSubEndo (mempty `mappend` y) y
                            && eqSubEndo (mconcat [x, y, z])
                                         (foldr mappend mempty [x, y, z])
                            && eqSubEndo ((x `mappend` y) `mappend` z)
                                         (x `mappend` (y `mappend` z))

test53 =
    "\n3|  Builder"
    ++ "\n    fromString: "
    ++ verdict (fromString "" == Many []
                && fromString "abc" == Many [One 'a', One 'b', One 'c'])
    ++ "\n    toString: "
    ++ verdict (toString (Many []) == ""
                && toString (One 'x') == "x"
                && toString (Many [One 'x']) == "x"
                && toString (Many [One 'a', One 'b', One 'c', One 'd']) == "abcd"
                && toString (Many [Many [One 'a', One 'b'], One 'c', One 'd'])
                   == "abcd"
                && toString (Many [Many [One 'a', One 'b'], Many [One 'c', One 'd']])
                   == "abcd")
    ++ "\n    <>: "
    ++ verdict ((oneBuild <> manyOneBuild) <> manyMultiBuild
                == oneBuild <> (manyOneBuild <> manyMultiBuild))
    ++ "\n    mempty, mappend: "
    ++ verdict (oneBuild `mappend` mempty == oneBuild
                && mempty `mappend` manyOneBuild == manyOneBuild
                && mconcat [oneBuild, manyOneBuild, manyMultiBuild]
                == foldr mappend mempty [oneBuild, manyOneBuild, manyMultiBuild]
                && (oneBuild `mappend` manyOneBuild) `mappend` manyMultiBuild
                == oneBuild `mappend` (manyOneBuild `mappend` manyMultiBuild))
  where
    oneBuild = One 'a'
    manyOneBuild = Many [One 'b', One 'c']
    manyMultiBuild = Many [Many [One 'd'], One 'e']
