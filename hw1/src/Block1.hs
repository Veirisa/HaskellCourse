module Block1 where

import           Data.List (sort)

------------------------------ TASK 1 ------------------------------

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) = let [e1, e2, e3] = sort [x, y, z] in (e1, e2, e3)

------------------------------ TASK 2 ------------------------------

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

------------------------------ TASK 3 ------------------------------

contains :: Eq a => a -> [[a]] -> [[a]]
contains x = filter (elem x)

------------------------------ TASK 4 ------------------------------

stringSum :: String -> Int
stringSum s = sum (map read (words s))
