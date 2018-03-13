module Block2 where

import           Data.List (length, reverse)

------------------------------ TASK 1 ------------------------------

type Index = Int
type ListWithoutElement a = [a]

removeFromList :: Index -> [a] -> (a, ListWithoutElement a)
removeFromList ind l = (l !! ind, take ind l ++ drop (ind + 1) l)

------------------------------ TASK 2 ------------------------------

mergeSort :: Ord a => [a] -> [a]
mergeSort l = mergeParse (split l)
  where
    split :: [a] -> ([a], [a])
    split l = let mid = div (length l) 2 in (take mid l, drop mid l)

    merge :: Ord a => [a] -> [a] -> [a] -> [a]
    merge acc (x : xs) (y : ys) =
        if x < y
        then merge (x : acc) xs (y : ys)
        else merge (y : acc) (x : xs) ys
    merge acc (x : xs) _ = merge (x : acc) xs []
    merge acc _ (y : ys) = merge (y : acc) [] ys
    merge acc _ _ = reverse acc

    mergeParse :: Ord a => ([a], [a]) -> [a]
    mergeParse ([], ly) = ly
    mergeParse (lx, []) = lx
    mergeParse (lx, ly) =
        merge [] (mergeParse (split lx)) (mergeParse (split ly))
