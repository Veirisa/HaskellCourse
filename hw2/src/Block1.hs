{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Block1 where

import           Control.Monad    (liftM2)
import           Data.Either      (fromRight)

import           Test.Tasty       (TestTree)
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import           Hedgehog         hiding (eval)
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range

------------------------------ TASK 1 ------------------------------

data Expr = Const Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr

data ArithmeticError = DivError | PowError
    deriving Eq

instance Show ArithmeticError where
    show :: ArithmeticError -> String
    show DivError = "The expression contains division by 0"
    show PowError = "The expression contains a negative exponentiation"

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = return x
eval (Add l r) = liftM2 (+) (eval l) (eval r)
eval (Sub l r) = liftM2 (-) (eval l) (eval r)
eval (Mul l r) = liftM2 (*) (eval l) (eval r)
eval (Div l r) =
  let
    rCalced = eval r
    isNewError = fromRight 1 rCalced == 0
  in
    liftM2 div (eval l) (if isNewError then Left DivError else rCalced)
eval (Pow l r) =
  let
    rCalced = eval r
    isNewError = fromRight 1 rCalced < 0
  in
    liftM2 (^) (eval l) (if isNewError then Left PowError else rCalced)

------- Testing (unit):

test11 :: IO ()
test11 = hspecTestTree11 >>= \unitTests -> defaultMain unitTests

hspecTestTree11 :: IO TestTree
hspecTestTree11 = testSpec "━━━ Block1 - Task1 ━━━" spec11

spec11 :: Spec
spec11 = do
  describe "eval works" $ do
    it "eval on const input" $
      eval constExpr `shouldBe` Right 42
    it "eval on short input" $
      eval shortExpr `shouldBe` Right (-2)
    it "eval on medium input" $
      eval mediumExpr `shouldBe` Right 81
    it "eval on long input" $
      eval longExpr `shouldBe` Right 0
    it "eval on input with division by 0" $
      eval divByZeroExpr `shouldBe` Left DivError
    it "eval on input with negative exponentiation" $
      eval negExpExpr `shouldBe` Left PowError
  where
    constExpr = Const 42
    shortExpr = Div (Const 10) (Const (-5))
    mediumExpr = Pow (Sub (Const 10) (Const 7)) (Mul (Const (-1)) (Const (-4)))
    longExpr =
      Sub (Mul (Div (Const 9) (Const 4)) (Const 2)) (Add (Const (-4)) (Pow (Const 2) (Const 3)))
    divByZeroExpr = Mul (Div (Const 9) (Const 0)) (Const 2)
    negExpExpr = Add (Const (-4)) (Pow (Const 2) (Const (-3)))

------------------------------ TASK 2 ------------------------------

bin n = foldr ($) [[]] (replicate n (>>= \s -> [0 : s, 1 : s]))

------- Testing (property-based):

test12 :: IO Bool
test12 =
  checkParallel $ Group "Block1 - Task2" [
      ("prop_binAmount", prop_binAmount),
      ("prop_binSeqElements", prop_binSeqElements)
    ]

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 15)

prop_binAmount :: Property
prop_binAmount = property $
    forAll genInt >>= \n -> length (bin n) === (2 ^ n)

prop_binSeqElements :: Property
prop_binSeqElements = property $
    forAll genInt >>= \n -> all (checkSeq n) (bin n) === True
  where
    checkSeq :: Int -> [Int] -> Bool
    checkSeq n l = (length l == n) && all (\x -> x == 0 || x == 1) l
