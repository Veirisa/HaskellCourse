module Main where

import           Control.Monad.Trans.Reader (Reader, ask, runReader)
import           Data.Int                   (Int64)

getAnswer :: Int64 -> Int64 -> Reader (Int64, Int64, Int64, Int64) Int64
getAnswer ans 1 = return ans
getAnswer ans x = do
    (n, k, a, b) <- ask
    let xdivk = div x k
    if xdivk == 0
    then getAnswer (ans + (x - 1) * a) 1
    else
        if mod x k == 0
        then do
            let change = x - xdivk
            if change * a > b
            then getAnswer (ans + b) xdivk
            else getAnswer (ans + change * a) xdivk
        else do
            let dif = x - k * xdivk
            getAnswer (ans + dif * a) (k * xdivk)

main :: IO ()
main = do
    nStr <- getLine
    let n = read nStr :: Int64
    kStr <- getLine
    let k = read kStr :: Int64
    aStr <- getLine
    let a = read aStr :: Int64
    bStr <- getLine
    let b = read bStr :: Int64
    if k == 1
    then print $ (n - 1) * a
    else print $ runReader (getAnswer 0 n) (n, k, a, b)
