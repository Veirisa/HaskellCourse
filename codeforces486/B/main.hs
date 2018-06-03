module Main where

import           Control.Monad (replicateM)
import           Data.List     (isInfixOf, sort)

newtype MyString = MyString String
    deriving (Eq)

instance Show MyString where
    show (MyString a) = a

instance Ord MyString where
    (<=) (MyString a) (MyString b) = length a <= length b && isInfixOf a b

checkInvariant :: [MyString] -> Bool
checkInvariant l@(x : xs) = all (\(MyString a, MyString b) -> isInfixOf a b) (zip l xs)
checkInvariant _          = True

main :: IO ()
main = do
    nStr <- getLine
    let n = read nStr :: Int
    s <- replicateM n getLine
    let answer = sort (map MyString s)
    if checkInvariant answer
    then do
        putStrLn "YES"
        mapM_ print answer
    else putStrLn "NO"
