{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Block2 where

import           Control.Monad    (liftM2)
import           Data.Char        (isDigit)
import           Data.Maybe       (isJust, isNothing)

import           Test.Tasty       (TestTree)
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import           Hedgehog
import qualified Hedgehog.Gen     as Gen

------------------------------ TASK 1 ------------------------------

stringSum :: String -> Maybe Int
stringSum s = saveSum (map tryRead (words s))
  where
    tryRead :: String -> Maybe Int
    tryRead s@(x : xs) =
        if ((x == '-' && not (null xs)) || isDigit x) && all isDigit xs
        then Just (read s)
        else Nothing
    tryRead _ = error "impossible situation"

    saveSum :: [Maybe Int] -> Maybe Int
    saveSum a = foldr (liftM2 (+)) (Just 0) a

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
      ("prop_incorrectSymbols", prop_incorrectSymbols),
      ("prop_minuses", prop_minuses)
    ]

genIncorrect :: Gen [Char]
genIncorrect = Gen.shuffle "\n\n\t\t  .0123456789--"

prop_incorrectSymbols :: Property
prop_incorrectSymbols = property $
    forAll genIncorrect >>= \s -> stringSum s === Nothing

genCorrect :: Gen [Char]
genCorrect = Gen.shuffle "\n\n\t\t  0123456789--"

prop_minuses :: Property
prop_minuses = property $
    forAll genCorrect >>= \s ->
        if checkMinuses True s
        then isJust (stringSum s) === True
        else isNothing (stringSum s) === True
  where
    checkMinuses :: Bool -> String -> Bool
    checkMinuses wasSpace (x1 : x2 : xs) =
      ((x1 /= '-') || (wasSpace && isDigit x2)) &&
      checkMinuses (x1 /= '-' && not (isDigit x1)) (x2 : xs)
    checkMinuses wasSpace (x : []) = x /= '-'
    checkMinuses _ _ = True

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

-- instance Traversable Optional where

------- Testing (ER):


-- 1. return a >>= f ≡ f a (left identity)

-- return a >>= f = Optional (return (return a)) >>= f  (1): return
--                = Optional (Just (Just a)) >>= f      (2): return (Maybe)
--                = f a                                 (3): bind - Optional (Just (Just a))


-- 2. m >>= return ≡ m (right identity)

-- Optional Nothing >>= return = Optional Nothing                                         (1): bind - Optional Nothing

-- Optional (Just Nothing) >>= return = Optional (Just Nothing)                           (1): bind - Optional (Just Nothing)

-- Optional (Just (Just Nothing)) >>= return = return Nothing                             (1): bind - Optional (Just (Just Nothing))
--                                           = return Optional (return (return Nothing))  (2): return (Maybe)
--                                           = return Optional (Just (Just Nothing))      (3): return


-- 3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g) (associativity)
