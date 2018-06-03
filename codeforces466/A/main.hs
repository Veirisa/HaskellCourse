module Main where

tryChooseMin :: Int -> [Int] -> Int -> Int
tryChooseMin d l x = length $ filter (\el -> el < x || abs(el - x) > d) l

main :: IO ()
main = do
  first <- getLine
  let [n, d] = map read (words first) :: [Int]
  second <- getLine
  let l = map read (words second) :: [Int]
  let ansL = map (tryChooseMin d l) l
  print $ foldr min n ansL
