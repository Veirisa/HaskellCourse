module Main where

import           Data.List (dropWhileEnd)

main :: IO ()
main = do
  first <- getLine
  let [n, k] = map read (words first) :: [Int]
  s <- getLine
  let fakeMaxChar = succ 'z'
  let minChar = foldr min fakeMaxChar s
  let fakeMinChar = pred 'a'
  let maxChar = foldr max fakeMinChar s
  if n < k
  then putStrLn $ s ++ replicate (k - n) minChar
  else do
      let bufS = take k s
      let newS = dropWhileEnd ((==) maxChar) bufS
      let newLen = length newS
      let lastChar = newS !! (newLen - 1)
      let filtNewS = filter ((<) lastChar) s
      let minSpecialChar = foldr min fakeMaxChar filtNewS
      putStrLn $ take (newLen - 1) newS ++ [minSpecialChar] ++ replicate (k - newLen) minChar
