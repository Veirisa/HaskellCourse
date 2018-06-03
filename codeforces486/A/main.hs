module Main where

import           Control.Monad.State (State, get, put, runState)
import qualified Data.Set            as S (Set, fromList, insert, notMember)

getAnsSet :: [(Int, Int)] -> State ([Int], S.Set Int) ()
getAnsSet = mapM_ (\(x, ind) -> do
                      (ans, set) <- get
                      if S.notMember x set
                      then put (ind : ans, S.insert x set)
                      else return ())

main :: IO ()
main = do
  first <- getLine
  let [n, k] = map read (words first) :: [Int]
  second <- getLine
  let a = map read (words second) :: [Int]
  let n = length a
  let nums = [1..n]
  let l = zip a nums
  let ansList = fst (snd (runState (getAnsSet l) ([], S.fromList [])))
  if (length ansList >= k)
  then do
      putStrLn "YES"
      mapM_ (\el -> putStr (show el ++ " ")) (take k ansList)
  else putStrLn "NO"
