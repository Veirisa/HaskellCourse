{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Block2 where

import           Control.Monad    (liftM2)
import           Data.Char        (isDigit, isSpace)
import           Data.Maybe       (isJust, isNothing)
import           Text.Read        (readMaybe)

import           Test.Tasty       (TestTree)
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import           Hedgehog
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range

------------------------------ TASK 1 ------------------------------

stringSum :: String -> Maybe Int
stringSum s = foldr (liftM2 (+)) (Just 0) (map readMaybe (words s))

------- Testing (unit):

testUnit21 :: IO ()
testUnit21 = hspecTestTree21 >>= \unitTests -> defaultMain unitTests

hspecTestTree21 :: IO TestTree
hspecTestTree21 = testSpec "━━━ Block2 - Task1 ━━━" spec21

spec21 :: Spec
spec21 = do
  describe "stringSum works" $ do
    it "stringSum on correct inputs" $
      map stringSum passTests `shouldBe` passAnswers
    it "stringSum on incorrect inputs" $
      map stringSum mustFail `shouldBe` mustFailAnswers
  where
    passTests = ["1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030",
                 " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 ",
                 "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"]
    passAnswers = map Just [1, 6, 1, 1, 1, 12345, 60, 1368, -1, -6, -12345,
                            -1368, 553, 400] :: [Maybe Int]
    mustFail = ["asd", "1-1", "1.2", "--2", "+1", "1+", "- 2"]
    mustFailAnswers = replicate 7 Nothing

------- Testing (property-based):

testProp21 :: IO Bool
testProp21 =
  checkParallel $ Group "Block2 - Task1" [
      ("prop_stringSumJustOrNothing", prop_stringSumJustOrNothing)
    ]

genChar :: Gen Char
genChar = Gen.element "-123 \t\n.a^)"

genString :: Char -> Gen String
genString c = Gen.shuffle (c : "\n\n\t\t  0123456789--")

prop_stringSumJustOrNothing :: Property
prop_stringSumJustOrNothing = property $
    forAll genChar >>= \c -> forAll (genString c) >>= \s ->
        if all correctSymbol s && correctMinuses True s
        then isJust (stringSum s) === True
        else isNothing (stringSum s) === True
  where
    correctSymbol :: Char -> Bool
    correctSymbol c = (c == '-') || isSpace c || isDigit c

    correctMinuses :: Bool -> String -> Bool
    correctMinuses wasSpace (x1 : x2 : xs) =
      ((x1 /= '-') || (wasSpace && isDigit x2)) &&
      correctMinuses (x1 /= '-' && not (isDigit x1)) (x2 : xs)
    correctMinuses wasSpace (x : []) = x /= '-'
    correctMinuses _ _ = True

------------------------------ TASK 2 ------------------------------
-- реализовать Traversable

data Optional a = Optional (Maybe (Maybe a))
    deriving (Show, Eq)

instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap f (Optional (Just mx)) = Optional (Just (fmap f mx))
    fmap _ _                    = Optional Nothing

instance Applicative Optional where
    pure :: a -> Optional a
    pure x = Optional (pure (pure x))

    (<*>) :: Optional (a -> b) -> Optional a -> Optional b
    Optional (Just (Just f)) <*> opx = fmap f opx
    Optional (Just Nothing) <*> _    = Optional (Just Nothing)
    Optional Nothing <*> _           = Optional Nothing

instance Monad Optional where
    return :: a -> Optional a
    return x = Optional (return (return x))                     -- (1)

    (>>=) :: Optional a -> (a -> Optional b) -> Optional b
    Optional (Just (Just x)) >>= f = f x                        -- (2)
    Optional (Just Nothing) >>= _  = Optional (Just Nothing)    -- (3)
    _ >>= _                        = Optional Nothing           -- (4)

instance Foldable Optional where
    foldr :: (a -> b -> b) -> b -> Optional a -> b
    foldr f y (Optional (Just (Just x))) = f x y
    foldr f y _                          = y

instance Traversable Optional where
    traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
    traverse = undefined

------- Testing (ER):

-- 1. return a >>= f ≡ f a                          - left identity

-- return a >>= f
--     = Optional (return (return a)) >>= f         (1): return
--     = Optional (Just (Just a)) >>= f             return /Maybe/
--     = f a                                        (2): bind - Optional (Just (Just a))


-- 2. m >>= return ≡ m                              - right identity

-- Optional Nothing >>= return
--     = Optional Nothing                           (4): bind - Optional Nothing

-- Optional (Just Nothing) >>= return
--     = Optional (Just Nothing)                    (3): bind - Optional (Just Nothing)

-- Optional (Just (Just a)) >>= return
--     = return a                                   (2): bind - Optional (Just (Just a))
--     = Optional (return (return a))               (1): return
--     = Optional (Just (Just a))                   return /Maybe/


-- 3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)     - associativity

-- (Optional Nothing >>= f) >>= g
--     = Optional Nothing >>= g                     (4): bind - Optional Nothing
--     = Optional Nothing                           (4): bind - Optional Nothing
-- Optional Nothing >>= (\x -> f x >>= g)
--     = Optional Nothing                           (4): bind - Optional Nothing

-- (Optional (Just Nothing) >>= f) >>= g
--     = Optional (Just Nothing) >>= g              (3): bind - Optional (Just Nothing)
--     = Optional (Just Nothing)                    (3): bind - Optional (Just Nothing)
-- Optional (Just Nothing) >>= (\x -> f x >>= g)
--     = Optional (Just Nothing)                    (3): bind - Optional (Just Nothing)

-- (Optional (Just (Just a)) >>= f) >>= g
--     = f a >>= g                                  (2): bind - Optional (Just (Just a))
-- Optional (Just (Just a)) >>= (\x -> f x >>= g)
--     = (\x -> f x >>= g) a                        (2): bind - Optional (Just (Just a))
--     = f a >>= g                                  function application

------------------------------ TASK 3 ------------------------------
-- реализовать Traversable

data NonEmpty a = a :| [a]
    deriving (Show, Eq)

instance Functor NonEmpty where
    fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
    fmap f (x :| xs) = f x :| map f xs

instance Applicative NonEmpty where
    pure :: a -> NonEmpty a
    pure x = x :| []

    (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
    (f :| fs) <*> (x :| xs) =
        f x :| (map f xs ++ [fi xi | fi <- fs, xi <- (x : xs)])

instance Monad NonEmpty where
    return :: a -> NonEmpty a
    return x = x :| []

    (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
    (x :| xs) >>= f  = f x `unite` concat (map (toList . f) xs)
      where
        toList :: NonEmpty a -> [a]
        toList (y :| ys) = y : ys

        unite :: NonEmpty a -> [a] -> NonEmpty a
        unite (y :| ys) l = y :| (ys ++ l)

instance Foldable NonEmpty where
    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f z (x1 :| (x2 : xs)) = f x1 (foldr f z (x2 :| xs))
    foldr f z (x :| _)          = f x z

instance Traversable NonEmpty where
    traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
    traverse = undefined

------- Testing (property-based):

testProp23 :: IO Bool
testProp23 =
  checkParallel $ Group "Block2 - Task3" [
      ("prop_monadNonEmptyFirstLaw", prop_monadNonEmptyFirstLaw),
      ("prop_monadNonEmptySecondLaw", prop_monadNonEmptySecondLaw),
      ("prop_monadNonEmptyThirdLaw", prop_monadNonEmptyThirdLaw)
    ]

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 100)

genIntList :: Gen [Int]
genIntList =
  let
    listLength = Range.linear 0 1000
  in
    Gen.list listLength Gen.enumBounded

genFunc :: Gen (Int -> Int)
genFunc = Gen.element [(*3), (^2), (+5), negate, signum, abs, id]

combineFuncs :: (Int -> Int) -> (Int -> Int) -> (Int -> NonEmpty Int)
combineFuncs f1 f2 = combFunc
  where
    combFunc :: Int -> NonEmpty Int
    combFunc x = (f1 . f2) x :| [f1 x, f2 x]

-- 1. return a >>= f ≡ f a                          - left identity
prop_monadNonEmptyFirstLaw :: Property
prop_monadNonEmptyFirstLaw = property $
    forAll genInt >>= \a -> forAll genFunc >>= \f1 -> forAll genFunc >>= \f2 ->
      let
        f = combineFuncs f1 f2
      in
        (return a >>= f) === f a

-- 2. m >>= return ≡ m                              - right identity
prop_monadNonEmptySecondLaw :: Property
prop_monadNonEmptySecondLaw = property $
    forAll genInt >>= \x -> forAll genIntList >>= \xs ->
      let
        m = x :| xs
      in
        (m >>= return) === m

-- 3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)     - associativity
prop_monadNonEmptyThirdLaw :: Property
prop_monadNonEmptyThirdLaw = property $
    forAll genInt >>= \x -> forAll genIntList >>= \xs ->
    forAll genFunc >>= \f1 -> forAll genFunc >>= \f2 ->
    forAll genFunc >>= \g1 -> forAll genFunc >>= \g2 ->
      let
        m = x :| xs
        f = combineFuncs f1 f2
        g = combineFuncs g1 g2
      in
        ((m >>= f) >>= g) === (m >>= (\x -> f x >>= g))
