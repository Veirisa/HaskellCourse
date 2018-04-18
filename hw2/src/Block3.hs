{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Block3 where

import           Control.Applicative (Alternative, empty, (<|>))
import           Data.Char           (isDigit, isSpace)
import           Data.Functor        (($>))
import           Data.Maybe          (isJust, isNothing)

import           Test.Tasty          (TestTree, defaultMain)
import           Test.Tasty.Hspec    (Spec, describe, it, shouldBe,
                                      shouldSatisfy, testSpec)

------------------------------ TASK 1 ------------------------------

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser parser) = Parser (fmap (first f) . parser)
      where
        first :: (a -> b) -> (a,c) -> (b,c)
        first g (x, y) = (g x, y)

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure a = Parser $ \l -> Just (a, l)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    pf <*> pa = Parser $ \l -> case runParser pf l of
        Nothing     -> Nothing
        Just (f, t) -> case runParser pa t of
            Nothing     -> Nothing
            Just (a, r) -> Just (f a, r)

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser $ const Nothing

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    pa1 <|> pa2 = Parser $ \l -> case runParser pa1 l of
        Nothing -> runParser pa2 l
        res     -> res

instance Monad (Parser s) where
    return :: a -> Parser s a
    return a = Parser $ \l -> Just (a, l)

    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    pa >>= f = Parser $ \l -> case runParser pa l of
        Nothing     -> Nothing
        Just (a, r) -> runParser (f a) r

------------------------------ TASK 2 ------------------------------

ok :: Parser s ()
ok = Parser $ \l -> Just ((), l)

eof :: Parser s ()
eof = Parser $ \l -> case l of
    [] -> Just ((), l)
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \case
    []       -> Nothing
    (x : xs) -> if p x then Just (x, xs) else Nothing

element :: Eq s => s -> Parser s s
element el = satisfy (== el)

stream :: Eq s => [s] -> Parser s [s]
stream = foldr ((<*>) . fmap (:) . element) (pure [])

------- Testing (unit):

testUnit32 :: IO ()
testUnit32 = hspecTestTree32 >>= \unitTests -> defaultMain unitTests

hspecTestTree32 :: IO TestTree
hspecTestTree32 = testSpec "━━━ Block3 - Task2 ━━━" spec32

spec32 :: Spec
spec32 = do
  describe "ok works" $ do
    it "ok on \"\"" $
      runParser ok "" `shouldBe` Just((), "")
    it "ok on [1, 2, 3]" $
      runParser ok ([1, 2, 3] :: [Int]) `shouldBe` Just((), [1, 2, 3])
  describe "eof works" $ do
    it "eof on \"\"" $
      runParser eof "" `shouldBe` Just((), "")
    it "eof on [1, 2, 3]" $
      runParser eof ([1, 2, 3] :: [Int]) `shouldSatisfy` isNothing
  describe "satisfy works" $ do
    it "satisfy ('c'>) on \"\"" $
      runParser (satisfy ('c'>)) "" `shouldSatisfy` isNothing
    it "satisfy (isJust) on [Nothing, Just 0]" $
      runParser (satisfy isJust) [Nothing, Just (0 :: Int)]
      `shouldSatisfy` isNothing
    it "satisfy (\\x -> mod x 2 == 0) on [2, 3, 4]" $
      runParser (satisfy (\x -> mod x 2 == 0)) ([2, 3, 4] :: [Int])
      `shouldBe` Just(2, [3, 4])
  describe "element works" $ do
    it "element 'c' on \"\"" $
      runParser (element 'c') "" `shouldSatisfy` isNothing
    it "element Nothing on [Nothing, Just 0]" $
      runParser (element Nothing) [Nothing, Just (0 :: Int)]
      `shouldBe` Just(Nothing, [Just 0])
    it "element 1 on [2, 3, 4]" $
      runParser (element 1) ([2, 3, 4] :: [Int]) `shouldSatisfy` isNothing
  describe "stream works" $ do
    it "stream \"a\" on \"\"" $
      runParser (stream "a") "" `shouldSatisfy` isNothing
    it "stream \"ab\" on \"abcd\"" $
      runParser (stream "ab") "abcd" `shouldBe` Just("ab", "cd")
    it "stream [1, 2, 3] on [1, 2, 3]" $
      runParser (stream [1, 2, 3]) ([1, 2, 3] :: [Int])
      `shouldBe` Just([1, 2, 3], [])
    it "stream [4, 6] on [4, 5, 6]" $
      runParser (stream [4, 6]) ([4, 5, 6] :: [Int])
      `shouldSatisfy` isNothing

------------------------------ TASK 3 ------------------------------

brackets :: Parser Char ()
brackets = insideBrackets *> eof
  where
    insideBrackets :: Parser Char ()
    insideBrackets =
        (element '(' *> insideBrackets *> element ')' *> insideBrackets) <|> ok

getInt :: Parser Char Int
getInt = fmap read correctInt
  where
    digit :: Parser Char Char
    digit = satisfy isDigit

    number :: Parser Char String
    number = fmap (:) digit <*> (number <|> (ok $> ""))

    correctInt :: Parser Char String
    correctInt =
      let
        joinNotPlus x = if x == '+' then id else (x:)
      in
        (fmap joinNotPlus (element '-' <|> element '+') <*> number) <|> number

------- Testing (unit):

testUnit33 :: IO ()
testUnit33 = hspecTestTree33 >>= \unitTests -> defaultMain unitTests

hspecTestTree33 :: IO TestTree
hspecTestTree33 = testSpec "━━━ Block3 - Task3 ━━━" spec33

spec33 :: Spec
spec33 = do
  describe "brackets works" $ do
    it "brackets on \"\"" $
      runParser brackets "" `shouldSatisfy` isJust
    it "brackets on \"(())\"" $
      runParser brackets "(())" `shouldSatisfy` isJust
    it "brackets on \"()()\"" $
      runParser brackets "()()" `shouldSatisfy` isJust
    it "brackets on long correct input" $
      runParser brackets "((()()))()(()()())(())" `shouldSatisfy` isJust
    it "brackets on long incorrect input" $
      runParser brackets "((()()))()(()(())(())" `shouldSatisfy` isNothing
  describe "getInt works" $ do
    it "getInt on \"\"" $
      runParser getInt "" `shouldSatisfy` isNothing
    it "getInt on \"0123\"" $
      runParser getInt "0123" `shouldBe` Just(123, "")
    it "getInt on \"45.6\"" $
      runParser getInt "45.6" `shouldBe` Just(45, ".6")
    it "getInt on \"--78\"" $
      runParser getInt "--78" `shouldSatisfy` isNothing
    it "getInt on \"-9\"" $
      runParser getInt "-9" `shouldBe` Just(-9, "")

------------------------------ TASK 4 ------------------------------

getIntLists :: Parser Char [[Int]]
getIntLists =
    (fmap (:) (skipSpaces *> getIntList) <*> getIntListsNext)
    <|> (eof $> [])
  where
    getIntListsNext :: Parser Char [[Int]]
    getIntListsNext =
        (fmap (:) (skipSpacesAndComma *> getIntList) <*> getIntListsNext)
        <|> ((skipSpaces *> eof) $> [])

    getIntList :: Parser Char [Int]
    getIntList = (getInt <* ok) >>= getSomeInts

    getSomeInts :: Int -> Parser Char [Int]
    getSomeInts 0 = pure []
    getSomeInts n =
        fmap (:) (skipSpacesAndComma *> getInt <* ok) <*> getSomeInts (n - 1)

    skipSpacesAndComma :: Parser Char ()
    skipSpacesAndComma = skipSpaces *> element ',' *> skipSpaces *> ok

    skipSpaces :: Parser Char ()
    skipSpaces = (satisfy isSpace *> skipSpaces) <|> ok

------- Testing (unit):

testUnit34 :: IO ()
testUnit34 = hspecTestTree34 >>= \unitTests -> defaultMain unitTests

hspecTestTree34 :: IO TestTree
hspecTestTree34 = testSpec "━━━ Block3 - Task4 ━━━" spec34

spec34 :: Spec
spec34 =
  describe "getIntLists works" $ do
    it "getIntLists on \"\"" $
      runParser getIntLists "" `shouldBe` Just([], "")
    it "getIntLists on \" 0,0 \"" $
      runParser getIntLists " 0,0 " `shouldBe` Just([[], []], "")
    it "getIntLists on \"2,  -1, a\"" $
      runParser getIntLists "2,  -1, a" `shouldSatisfy` isNothing
    it "getIntLists on \"1,1-\"" $
      runParser getIntLists "1,1-" `shouldSatisfy` isNothing
    it "getIntLists on \" , 1, 2\"" $
      runParser getIntLists " , 1, 2" `shouldSatisfy` isNothing
    it "getIntLists on medium correct input" $
      runParser getIntLists "4,2,   -5  ,  +1,  9  "
      `shouldBe` Just([[2, -5, 1, 9]], "")
    it "getIntLists on long correct input" $
      runParser getIntLists "2, 1,+10 , +0 , 3,5,-7, 2"
      `shouldBe` Just([[1, 10], [], [5, -7, 2]], "")
